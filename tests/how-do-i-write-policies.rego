################################################################################
# Tests extracted from
# <https://www.openpolicyagent.org/docs/how-do-i-write-policies.html>.
package fregot.tests.how_do_i_write_policies

################################################################################
# Data

sites = [
    {
        "region": "east",
        "name": "prod",
        "servers": [
            {
                "name": "web-0",
                "hostname": "hydrogen"
            },
            {
                "name": "web-1",
                "hostname": "helium"
            },
            {
                "name": "db-0",
                "hostname": "lithium"
            }
        ]
    },
    {
        "region": "west",
        "name": "smoke",
        "servers": [
            {
                "name": "web-1000",
                "hostname": "beryllium"
            },
            {
                "name": "web-1001",
                "hostname": "boron"
            },
            {
                "name": "db-1000",
                "hostname": "carbon"
            }
        ]
    },
    {
        "region": "west",
        "name": "dev",
        "servers": [
            {
                "name": "web-dev",
                "hostname": "nitrogen"
            },
            {
                "name": "db-dev",
                "hostname": "oxygen"
            }
        ]
    }
]

apps = [
    {
        "name": "web",
        "servers": ["web-0", "web-1", "web-1000", "web-1001", "web-dev"]
    },
    {
        "name": "mysql",
        "servers": ["db-0", "db-1000"]
    },
    {
        "name": "mongodb",
        "servers": ["db-dev"]
    }
]

containers = [
    {
        "image": "redis",
        "ipaddress": "10.0.0.1",
        "name": "big_stallman"
    },
    {
        "image": "nginx",
        "ipaddress": "10.0.0.2",
        "name": "cranky_euclid"
    }
]

################################################################################
# Generating Sets

hostnames[name] { sites[_].servers[_].hostname = name }

test_hostnames {
    hostnames["hydrogen"]
}

test_not_hostnames {
    not hostnames["bulbasaur"]
}

################################################################################
# Generating Objects

apps_by_hostname[hostname] = app {
    sites[_].servers[_] = server
    server.hostname = hostname
    apps[i].servers[_] = server.name
    apps[i].name = app
}

test_apps_by_hostname {
    app = apps_by_hostname["helium"]
    app == "web"
}

################################################################################
# Incremental definitions

instances[instance] {
    sites[_].servers[_] = server
    instance = {"address": server.hostname, "name": server.name}
}

instances[instance] {
    containers[_] = container
    instance = {"address": container.ipaddress, "name": container.name}
}

test_instances {
    # This checks that `x` gets properly unified.
    instances[x]
    x == {"address": "hydrogen", "name": "web-0"}
}

################################################################################
# Complete definitions

user = "alice"

power_users = {"alice", "bob", "fred"}
restricted_users = {"bob", "kim"}
max_memory = 32 { power_users[user] }
max_memory = 4 { restricted_users[user] }

test_max_memory {
    max_memory == 32
}

################################################################################
# Array comprehensions

test_array_comprehensions {
    region = "west"
    west_names = [name | sites[i].region = region; sites[i].name = name]
    west_names == ["smoke", "dev"]
}

prod_servers[name] = name {
    sites[_] = site
    site.name = "prod"
    site.servers[_].name = name
}

apps_in_prod[name] = name {
    apps[_] = app
    app.servers[_] = server
    app.name = name
    prod_servers[server]
}

apps_not_in_prod[name] = name {
    apps[_].name = name
    not apps_in_prod[name]
}

test_apps_in_prod {
    apps_in_prod["web"]
    apps_in_prod["mysql"]
}

same_site[apps[k].name] = apps[k].name {
    apps[i].name = "mysql"
    apps[i].servers[_] = server
    sites[j].servers[_].name = server
    sites[j].servers[_].name = other_server
    server != other_server
    apps[k].servers[_] = other_server
}
