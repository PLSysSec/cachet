var foo: int;

procedure {:entrypoint} bar(baz: int);
  ensures baz == 1;



implementation {:entrypoint} bar(baz: int)
{

  anon0:
    baz := 1;
    return;
}


