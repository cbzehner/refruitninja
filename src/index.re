open Reprocessing;

module Draw = {
  include Draw;
  let mapOption = (value, fn) =>
    switch (value) {
    | Some(x) => Some(fn(x))
    | None => None
    };

  let imagef = (img, ~pos, ~width=?, ~height=?, env) => {
    let (x, y) = pos;
    let x = int_of_float(x);
    let y = int_of_float(y);

    let width = mapOption(width, int_of_float);
    let height = mapOption(height, int_of_float);

    image(img, ~pos=(x, y), ~width?, ~height?, env);
  };
};

type fruitShape =
  | Circle
  | Rectangle;

type fruitType =
  | Orange
  | Coconut;

type fruit = {
  fruitType,
  fruitShape,
  fruitX: float,
  fruitY: float,
  fruitXV: float,
  fruitYV: float,
  sliced: bool,
};

type state = {fruits: list(fruit)};

let gravity = 5.;
let worldSize = 600;

let setup = env => {
  Env.size(~width=worldSize, ~height=worldSize, env);
  /* generateFruits should set these values and load the images */
  let fruit = {
    fruitType: Orange,
    fruitShape: Circle,
    /* These could be Tuples (x, y) */
    fruitX: 80., /* Random.float */
    fruitY: float_of_int(worldSize),
    fruitXV: 100.,
    fruitYV: (-80.) *. gravity,
    sliced: false,
  };
  {fruits: [fruit]};
};

let draw = ({fruits}, env) => {
  let timeStep = Env.deltaTime(env);
  let fruits =
    if (Env.keyPressed(Space, env)) {
      let {fruits} = setup(env);
      fruits;
    } else {
      fruits;
    };

  let fruit = List.hd(fruits);

  /* Do Stuff */
  let sliced = Env.mousePressed(env) || fruit.sliced;

  /* Move Stuff */
  let newX = fruit.fruitX +. timeStep *. fruit.fruitXV;
  let newY = fruit.fruitY +. timeStep *. fruit.fruitYV;
  let newYV = fruit.fruitYV +. gravity;

  let nextFruit = {
    ...fruit,
    fruitX: newX,
    fruitY: newY,
    fruitYV: newYV,
    sliced,
  };

  /* Draw Stuff */
  Draw.background(Utils.color(~r=199, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  Draw.text(~body=string_of_float(timeStep), ~pos=(10, 10), env);
  /* Add an explosion */
  let fruitImgs =
    switch (fruit.fruitType, sliced) {
    | (Orange, false) => [
        Draw.loadImage(~filename="./assets/orange.png", env),
      ]
    | (Orange, true) => [
        Draw.loadImage(~filename="./assets/orange_half_1.png", env),
        Draw.loadImage(~filename="./assets/orange_half_2.png", env),
      ]
    | (Coconut, false) => [
        Draw.loadImage(~filename="./assets/conconut.png", env),
      ]
    | (Coconut, true) => [
        Draw.loadImage(~filename="./assets/conconut_half_1.png", env),
        Draw.loadImage(~filename="./assets/conconut_half_2.png", env),
      ]
    };
  List.iter(
    img =>
      Draw.imagef(
        img,
        ~pos=(nextFruit.fruitX, nextFruit.fruitY),
        ~width=100.,
        ~height=100.,
        env,
      ),
    fruitImgs,
  );
  /*switch (fruit.fruitShape) {
      | Circle => Draw.ellipsef(~center=(nextFruit.fruitX, nextFruit.fruitY), ~radx=100., ~rady=100., env)
      | Rectangle => Draw.rectf(~pos=(nextFruit.fruitX, nextFruit.fruitY), ~width=100., ~height=100., env)
    };*/

  /* Return the State */
  {fruits: [nextFruit]};
};

run(~setup, ~draw, ());