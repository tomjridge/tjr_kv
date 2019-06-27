NAME:=tjr_kv

all:
	$(MAKE) opam_with_pinning

opam_with_pinning:
	docker build --no-cache -t $(NAME)_$@ -f Dockerfile.$@ .
