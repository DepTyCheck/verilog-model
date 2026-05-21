-- Seed: 14565373476394343812,11460338385561364835



entity exqzxqs is
  port (clhqeb : linkage real; ihgdtszz : out real);
end exqzxqs;



architecture apix of exqzxqs is
  
begin
  
end apix;



entity oca is
  port (nbi : buffer real; fmgpsm : in integer);
end oca;



architecture kubpyhcgt of oca is
  signal sykolae : real;
  signal akrp : real;
  signal jonyvplk : real;
begin
  znguntowm : entity work.exqzxqs
    port map (clhqeb => nbi, ihgdtszz => jonyvplk);
  pij : entity work.exqzxqs
    port map (clhqeb => akrp, ihgdtszz => nbi);
  tkfympiwtj : entity work.exqzxqs
    port map (clhqeb => sykolae, ihgdtszz => akrp);
end kubpyhcgt;



entity zwosdbke is
  port (inm : in real);
end zwosdbke;



architecture idqmeflxra of zwosdbke is
  signal ytifmrep : real;
  signal cgvlak : integer;
  signal ohivcxzlz : real;
begin
  luhodyctx : entity work.oca
    port map (nbi => ohivcxzlz, fmgpsm => cgvlak);
  cgqmmre : entity work.exqzxqs
    port map (clhqeb => ohivcxzlz, ihgdtszz => ytifmrep);
end idqmeflxra;



entity fraqqs is
  port (ogfoirs : linkage real; lvcxnxlvf : out real);
end fraqqs;



architecture xdrqxq of fraqqs is
  signal llaixv : real;
  signal krpqux : real;
  signal b : real;
begin
  g : entity work.zwosdbke
    port map (inm => b);
  fxmlzftlx : entity work.zwosdbke
    port map (inm => krpqux);
  xparxknv : entity work.exqzxqs
    port map (clhqeb => lvcxnxlvf, ihgdtszz => llaixv);
end xdrqxq;



-- Seed after: 7286983960996636826,11460338385561364835
