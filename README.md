# LearningHaskell

![Haskell](https://i.imgur.com/qhXXFbA.png)

Code examples for learning haskell

## Course

### Yorgey's cis1940 course

Available [online](https://www.seas.upenn.edu/~cis1940/spring13/lectures.html).

> [Brent Yorgey](https://byorgey.wordpress.com)'s course is the best I've found so
> far. This course is valuable as it will not only equip you to write basic
> Haskell but also help you to understand parser combinators.

## Tool

### IHaskell

![jupyter](https://i.imgur.com/S16l2Hw.png)

> [IHaskell](https://github.com/IHaskell/IHaskell) is a kernel for the [Jupyter project](https://jupyter.org),
> which allows you to use Haskell inside Jupyter frontends
> (including the console and notebook).

In order to mount your own local files into the Docker container
use following command:

```sh
docker run --rm -p 8888:8888 -v "$PWD":/home/jovyan/src gibiansky/ihaskell
```
