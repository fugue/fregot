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
