.base64_alphabet_3d <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

base64_3d <- function(bytes) {
  n <- length(bytes)
  if (!n) return("")
  pad <- (3L - n %% 3L) %% 3L
  b <- matrix(as.integer(c(bytes, raw(pad))), 3L)
  v <- b[1L, ] * 65536L + b[2L, ] * 256L + b[3L, ]
  index <- rbind(v %/% 262144L, (v %/% 4096L) %% 64L, (v %/% 64L) %% 64L, v %% 64L)
  out <- charToRaw(.base64_alphabet_3d)[index + 1L]
  if (pad) out[length(out) - seq_len(pad) + 1L] <- charToRaw("=")
  rawToChar(out)
}

base64_decode_3d <- function(text) {
  text <- gsub("[[:space:]]", "", text)
  if (!nzchar(text)) return(raw())
  chars <- strsplit(text, "", fixed = TRUE)[[1L]]
  pad <- sum(chars == "=")
  scene_assert(length(chars) %% 4L == 0L && pad <= 2L &&
    (!pad || all(tail(chars, pad) == "=")), "Invalid base64 padding")
  values <- match(chars[chars != "="], strsplit(.base64_alphabet_3d, "", fixed = TRUE)[[1L]]) - 1L
  scene_assert(!anyNA(values), "Invalid base64 data")
  m <- matrix(c(values, integer(pad)), 4L)
  v <- m[1L, ] * 262144L + m[2L, ] * 4096L + m[3L, ] * 64L + m[4L, ]
  bytes <- as.raw(rbind(v %/% 65536L, (v %/% 256L) %% 256L, v %% 256L))
  bytes[seq_len(length(bytes) - pad)]
}
