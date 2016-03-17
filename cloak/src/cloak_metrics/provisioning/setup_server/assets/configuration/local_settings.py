LOG_DIR = '/var/log/graphite'
SECRET_KEY = '$(date +%s | sha256sum | base64 | head -c 64)' # random salt
TIME_ZONE = 'Europe/Berlin'