-- Seed: 5500291020338057338,13854332967471039201



entity pamttb is
  port (cnaljch : inout boolean_vector(4 to 1); urcyo : in integer);
end pamttb;



architecture e of pamttb is
  
begin
  
end e;



entity npwue is
  port (xojmh : buffer boolean; bjqaqsa : in bit_vector(0 to 3); qst : in real_vector(4 downto 0); cjftumgs : inout time);
end npwue;



architecture hunc of npwue is
  signal gukx : boolean_vector(4 to 1);
  signal lrqrtqv : integer;
  signal kz : boolean_vector(4 to 1);
begin
  yzysatzvin : entity work.pamttb
    port map (cnaljch => kz, urcyo => lrqrtqv);
  jsqcw : entity work.pamttb
    port map (cnaljch => gukx, urcyo => lrqrtqv);
end hunc;



-- Seed after: 3161085313330851207,13854332967471039201
