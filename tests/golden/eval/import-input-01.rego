package import_input

import input as state
import input.pods.nginx as nginx

deny["SSL needs to be enabled"] {
    not nginx.ssl_enabled
}

deny["You need more pods"] {
    count(state.pods) < 100
}
