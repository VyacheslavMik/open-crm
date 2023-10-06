### Development mode

Fill up `open-crm.cldn` file and put it in your home directory.

```bash
docker-compose up
```

Run `sbcl` or `slime` from root of the project.

```common lisp
(load "load.lisp")
```

Open `localhost:4242` and if you get blank screen, reload it.
