#pragma once
#include "common.h"

namespace ggplot3d { namespace engine {
ScenePacket read_packet(const Rcpp::List& packet);
Camera read_camera(const Rcpp::List& camera);
Rcpp::List write_result(const CompiledScene& scene, const ScenePacket& packet,
                        const Viewport& viewport);
} }
