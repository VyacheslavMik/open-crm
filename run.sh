# to initialize db use next line
# chown postgres /data && su - postgres -c '/usr/lib/postgresql/11/bin/initdb -D /data'
git pull

mkdir -p $(pwd)/../image
mkdir -p $(pwd)/../data
mkdir -p $(pwd)/../user_data
cp ~/open-crm.cldn .

docker build -t open-crm .
docker rm -f open-crm || true
docker run -p 4343:4242 -d --name open-crm \
       --mount src="$(pwd)/../image",target=/image,type=bind \
       --mount src="$(pwd)/../data",target=/data,type=bind \
       --mount src="$(pwd)/../user_data",target=/user_data,type=bind \
	open-crm
