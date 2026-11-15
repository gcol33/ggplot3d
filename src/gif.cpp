#include <Rcpp.h>
#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdlib>
#include <vector>

namespace {

int paeth(int a, int b, int c) {
  const int p = a + b - c;
  const int pa = std::abs(p - a), pb = std::abs(p - b), pc = std::abs(p - c);
  if (pa <= pb && pa <= pc) return a;
  return pb <= pc ? b : c;
}

// One colour box of the median cut: a run of occupied histogram bins.
struct Box {
  int begin, end;
  double count;
};

constexpr int kBins = 1 << 15;

int bin_channel(int key, int axis) { return (key >> (10 - 5 * axis)) & 31; }

class LzwWriter {
 public:
  LzwWriter(std::vector<std::uint8_t>& out, int min_code_size)
      : out_(out), min_code_size_(min_code_size) {}

  void write(const std::vector<std::uint8_t>& indices) {
    const int clear = 1 << min_code_size_, stop = clear + 1;
    out_.push_back(static_cast<std::uint8_t>(min_code_size_));
    reset(clear);
    emit(clear);
    if (indices.empty()) {
      emit(stop);
      finish();
      return;
    }
    int prefix = indices[0];
    for (std::size_t i = 1; i < indices.size(); ++i) {
      const int byte = indices[i];
      const std::uint32_t key = (static_cast<std::uint32_t>(prefix) << 8) | static_cast<std::uint32_t>(byte);
      std::size_t slot = (key * 2654435761u) & kMask;
      while (keys_[slot] != kEmpty && keys_[slot] != key) slot = (slot + 1) & kMask;
      if (keys_[slot] == key) {
        prefix = codes_[slot];
        continue;
      }
      emit(prefix);
      if (next_ < 4096) {
        keys_[slot] = key;
        codes_[slot] = static_cast<std::int16_t>(next_);
        if (next_ == (1 << code_size_) && code_size_ < 12) ++code_size_;
        ++next_;
      } else {
        emit(clear);
        reset(clear);
      }
      prefix = byte;
    }
    emit(prefix);
    emit(stop);
    finish();
  }

 private:
  static constexpr std::size_t kSize = 1u << 14, kMask = kSize - 1u;
  static constexpr std::uint32_t kEmpty = 0xFFFFFFFFu;

  void reset(int clear) {
    std::fill(keys_.begin(), keys_.end(), kEmpty);
    code_size_ = min_code_size_ + 1;
    next_ = clear + 2;
  }

  // The decoder adds a table entry one code later than the encoder, so the
  // code width grows after the code that fills the current width is written.
  void emit(int code) {
    bits_ |= static_cast<std::uint32_t>(code) << nbits_;
    nbits_ += code_size_;
    while (nbits_ >= 8) {
      push(static_cast<std::uint8_t>(bits_ & 0xFFu));
      bits_ >>= 8;
      nbits_ -= 8;
    }
  }

  void push(std::uint8_t byte) {
    block_[block_size_++] = byte;
    if (block_size_ == 255) flush();
  }

  void flush() {
    if (block_size_ == 0) return;
    out_.push_back(static_cast<std::uint8_t>(block_size_));
    out_.insert(out_.end(), block_.begin(), block_.begin() + block_size_);
    block_size_ = 0;
  }

  void finish() {
    if (nbits_ > 0) push(static_cast<std::uint8_t>(bits_ & 0xFFu));
    bits_ = 0;
    nbits_ = 0;
    flush();
    out_.push_back(0);
  }

  std::vector<std::uint8_t>& out_;
  int min_code_size_;
  int code_size_ = 0, next_ = 0;
  std::uint32_t bits_ = 0;
  int nbits_ = 0;
  std::array<std::uint8_t, 255> block_{};
  int block_size_ = 0;
  std::vector<std::uint32_t> keys_ = std::vector<std::uint32_t>(kSize, kEmpty);
  std::vector<std::int16_t> codes_ = std::vector<std::int16_t>(kSize, 0);
};

void put16(std::vector<std::uint8_t>& out, int value) {
  out.push_back(static_cast<std::uint8_t>(value & 0xFF));
  out.push_back(static_cast<std::uint8_t>((value >> 8) & 0xFF));
}

}  // namespace

// Reverses the PNG scanline filters (ISO/IEC 15948, section 9) of an inflated
// 8-bit, non-interlaced image with `channels` samples per pixel and returns
// RGB samples; grey is expanded and alpha is composited over white.
// [[Rcpp::export(rng = false)]]
Rcpp::RawVector png_unfilter_cpp(Rcpp::RawVector data, int width, int height, int channels) {
  const std::size_t stride = static_cast<std::size_t>(width) * channels;
  if (static_cast<std::size_t>(data.size()) < (stride + 1) * height) Rcpp::stop("PNG image data is truncated");
  std::vector<std::uint8_t> previous(stride, 0), current(stride, 0);
  Rcpp::RawVector rgb(static_cast<R_xlen_t>(width) * height * 3);
  for (int y = 0; y < height; ++y) {
    const std::uint8_t* line = &data[(stride + 1) * y];
    const int filter = line[0];
    for (std::size_t i = 0; i < stride; ++i) {
      const int left = i >= static_cast<std::size_t>(channels) ? current[i - channels] : 0;
      const int up = previous[i];
      const int corner = i >= static_cast<std::size_t>(channels) ? previous[i - channels] : 0;
      int value = line[i + 1];
      switch (filter) {
        case 0: break;
        case 1: value += left; break;
        case 2: value += up; break;
        case 3: value += (left + up) / 2; break;
        case 4: value += paeth(left, up, corner); break;
        default: Rcpp::stop("PNG image data uses an unknown filter");
      }
      current[i] = static_cast<std::uint8_t>(value & 0xFF);
    }
    for (int x = 0; x < width; ++x) {
      const std::uint8_t* px = &current[static_cast<std::size_t>(x) * channels];
      int r, g, b, a = 255;
      if (channels >= 3) { r = px[0]; g = px[1]; b = px[2]; if (channels == 4) a = px[3]; }
      else { r = g = b = px[0]; if (channels == 2) a = px[1]; }
      const std::size_t o = (static_cast<std::size_t>(y) * width + x) * 3;
      rgb[o] = static_cast<Rbyte>((r * a + 255 * (255 - a) + 127) / 255);
      rgb[o + 1] = static_cast<Rbyte>((g * a + 255 * (255 - a) + 127) / 255);
      rgb[o + 2] = static_cast<Rbyte>((b * a + 255 * (255 - a) + 127) / 255);
    }
    std::swap(previous, current);
  }
  return rgb;
}

// Encodes RGB frames of one size as a looping GIF89a. All frames share one
// palette of at most `colours` entries from a median cut of the joint colour
// histogram, so a pixel keeps its index wherever its colour is unchanged.
// From the second frame on, pixels with the same index as on screen become
// the transparent index and each frame is cropped to the rectangle that
// changed; long transparent runs are what LZW compresses best.
// [[Rcpp::export(rng = false)]]
Rcpp::RawVector gif_encode_cpp(Rcpp::List frames, int width, int height,
                               Rcpp::IntegerVector delays, int colours) {
  const std::size_t n_pixels = static_cast<std::size_t>(width) * height;
  const int n_frames = frames.size();
  if (n_frames == 0) Rcpp::stop("no frames to encode");
  if (width < 1 || height < 1 || width > 65535 || height > 65535) Rcpp::stop("GIF frames must be 1 to 65535 pixels wide and high");
  colours = std::max(2, std::min(255, colours));

  std::vector<double> count(kBins, 0.0);
  std::vector<std::array<double, 3>> sum(kBins, {0.0, 0.0, 0.0});
  std::vector<const Rbyte*> pixels(n_frames);
  for (int f = 0; f < n_frames; ++f) {
    Rcpp::RawVector frame = frames[f];
    if (static_cast<std::size_t>(frame.size()) != n_pixels * 3) Rcpp::stop("animation frames must share one size");
    pixels[f] = RAW(frame);
    for (std::size_t i = 0; i < n_pixels; ++i) {
      const Rbyte* p = pixels[f] + 3 * i;
      const int key = ((p[0] >> 3) << 10) | ((p[1] >> 3) << 5) | (p[2] >> 3);
      count[key] += 1.0;
      sum[key][0] += p[0]; sum[key][1] += p[1]; sum[key][2] += p[2];
    }
  }

  std::vector<int> occupied;
  for (int key = 0; key < kBins; ++key) if (count[key] > 0) occupied.push_back(key);
  std::vector<Box> boxes{{0, static_cast<int>(occupied.size()), 0.0}};
  for (int key : occupied) boxes[0].count += count[key];
  auto range = [&](const Box& box, int axis) {
    int lo = 31, hi = 0;
    for (int i = box.begin; i < box.end; ++i) {
      const int v = bin_channel(occupied[i], axis);
      lo = std::min(lo, v); hi = std::max(hi, v);
    }
    return hi - lo;
  };
  while (static_cast<int>(boxes.size()) < colours) {
    int best = -1, best_axis = 0;
    double best_score = 0.0;
    for (std::size_t b = 0; b < boxes.size(); ++b) {
      if (boxes[b].end - boxes[b].begin < 2) continue;
      for (int axis = 0; axis < 3; ++axis) {
        const double score = range(boxes[b], axis) * boxes[b].count;
        if (score > best_score) { best_score = score; best = static_cast<int>(b); best_axis = axis; }
      }
    }
    if (best < 0) break;
    Box box = boxes[best];
    std::sort(occupied.begin() + box.begin, occupied.begin() + box.end, [&](int a, int b) {
      return bin_channel(a, best_axis) < bin_channel(b, best_axis);
    });
    double half = 0.0;
    int split = box.begin;
    while (split < box.end - 1 && half + count[occupied[split]] <= box.count / 2) half += count[occupied[split++]];
    if (split == box.begin) half += count[occupied[split++]];
    boxes[best] = {box.begin, split, half};
    boxes.push_back({split, box.end, box.count - half});
  }

  const int n_colours = static_cast<int>(boxes.size());
  std::vector<std::array<int, 3>> palette(n_colours);
  std::vector<std::uint8_t> lookup(kBins, 0);
  for (int c = 0; c < n_colours; ++c) {
    double total[3] = {0.0, 0.0, 0.0};
    for (int i = boxes[c].begin; i < boxes[c].end; ++i)
      for (int k = 0; k < 3; ++k) total[k] += sum[occupied[i]][k];
    for (int k = 0; k < 3; ++k) palette[c][k] = static_cast<int>(total[k] / boxes[c].count + 0.5);
  }
  for (int key : occupied) {
    double mean[3];
    for (int k = 0; k < 3; ++k) mean[k] = sum[key][k] / count[key];
    int best = 0;
    double best_d = 1e300;
    for (int c = 0; c < n_colours; ++c) {
      const double dr = mean[0] - palette[c][0], dg = mean[1] - palette[c][1], db = mean[2] - palette[c][2];
      const double d = 2 * dr * dr + 4 * dg * dg + 3 * db * db;
      if (d < best_d) { best_d = d; best = c; }
    }
    lookup[key] = static_cast<std::uint8_t>(best);
  }

  const int transparent = n_colours;
  int table_bits = 1;
  while ((1 << table_bits) < n_colours + 1) ++table_bits;
  const int min_code_size = std::max(2, table_bits);

  std::vector<std::uint8_t> out = {'G', 'I', 'F', '8', '9', 'a'};
  put16(out, width);
  put16(out, height);
  out.push_back(static_cast<std::uint8_t>(0x80 | ((table_bits - 1) << 4) | (table_bits - 1)));
  out.push_back(0);
  out.push_back(0);
  for (int c = 0; c < (1 << table_bits); ++c)
    for (int k = 0; k < 3; ++k) out.push_back(c < n_colours ? static_cast<std::uint8_t>(palette[c][k]) : 0);
  const char loop[] = "NETSCAPE2.0";
  out.insert(out.end(), {0x21, 0xFF, 11});
  out.insert(out.end(), loop, loop + 11);
  out.insert(out.end(), {3, 1, 0, 0, 0});

  std::vector<std::uint8_t> screen(n_pixels), indices(n_pixels), cropped;
  for (int f = 0; f < n_frames; ++f) {
    for (std::size_t i = 0; i < n_pixels; ++i) {
      const Rbyte* p = pixels[f] + 3 * i;
      indices[i] = lookup[((p[0] >> 3) << 10) | ((p[1] >> 3) << 5) | (p[2] >> 3)];
    }
    int x0 = 0, y0 = 0, x1 = width - 1, y1 = height - 1;
    if (f > 0) {
      x0 = width; y0 = height; x1 = -1; y1 = -1;
      for (int y = 0; y < height; ++y)
        for (int x = 0; x < width; ++x) {
          const std::size_t i = static_cast<std::size_t>(y) * width + x;
          if (indices[i] != screen[i]) { x0 = std::min(x0, x); x1 = std::max(x1, x); y0 = std::min(y0, y); y1 = std::max(y1, y); }
        }
      if (x1 < 0) { x0 = x1 = y0 = y1 = 0; }
    }
    cropped.clear();
    for (int y = y0; y <= y1; ++y)
      for (int x = x0; x <= x1; ++x) {
        const std::size_t i = static_cast<std::size_t>(y) * width + x;
        cropped.push_back(f > 0 && indices[i] == screen[i] ? static_cast<std::uint8_t>(transparent) : indices[i]);
        screen[i] = indices[i];
      }
    const int delay = std::max(0, std::min(65535, static_cast<int>(delays[f % delays.size()])));
    out.insert(out.end(), {0x21, 0xF9, 4, static_cast<std::uint8_t>((1 << 2) | (f > 0 ? 1 : 0))});
    put16(out, delay);
    out.push_back(static_cast<std::uint8_t>(transparent));
    out.push_back(0);
    out.push_back(0x2C);
    put16(out, x0);
    put16(out, y0);
    put16(out, x1 - x0 + 1);
    put16(out, y1 - y0 + 1);
    out.push_back(0);
    LzwWriter(out, min_code_size).write(cropped);
  }
  out.push_back(0x3B);
  Rcpp::RawVector result(out.size());
  std::copy(out.begin(), out.end(), result.begin());
  return result;
}
