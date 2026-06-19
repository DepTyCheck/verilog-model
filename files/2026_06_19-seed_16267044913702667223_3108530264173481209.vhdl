-- Seed: 16267044913702667223,3108530264173481209

entity aykqr is
  port (p : buffer string(4 to 5); cvbcyl : out severity_level; lttdd : out time_vector(4 to 3); fxeyeot : buffer integer_vector(0 downto 2));
end aykqr;

architecture dopvgtdvl of aykqr is
  
begin
  
end dopvgtdvl;

entity gk is
  port (xhkncfdc : in time; snevveksw : buffer integer; uyg : linkage time; f : in real);
end gk;

architecture ipgq of gk is
  
begin
  -- Single-driven assignments
  snevveksw <= 1413;
end ipgq;

entity rqkfag is
  port (lqjo : buffer real);
end rqkfag;

architecture iyseluz of rqkfag is
  signal orrqi : real;
  signal qysxjywg : time;
  signal f : integer;
  signal qr : real;
  signal rlny : integer;
  signal fisurkp : time;
begin
  gfaurc : entity work.gk
    port map (xhkncfdc => fisurkp, snevveksw => rlny, uyg => fisurkp, f => qr);
  hecoa : entity work.gk
    port map (xhkncfdc => fisurkp, snevveksw => f, uyg => qysxjywg, f => orrqi);
end iyseluz;

entity yuqlox is
  port (hgk : in integer);
end yuqlox;

architecture ukigh of yuqlox is
  signal tcrvzegx : real;
  signal qz : integer_vector(0 downto 2);
  signal py : time_vector(4 to 3);
  signal bfkjbtw : severity_level;
  signal xrpvday : string(4 to 5);
  signal sdlmykc : integer_vector(0 downto 2);
  signal l : time_vector(4 to 3);
  signal wxpkkfnp : severity_level;
  signal lxlve : string(4 to 5);
begin
  dvvow : entity work.aykqr
    port map (p => lxlve, cvbcyl => wxpkkfnp, lttdd => l, fxeyeot => sdlmykc);
  pxlfdtu : entity work.aykqr
    port map (p => xrpvday, cvbcyl => bfkjbtw, lttdd => py, fxeyeot => qz);
  ovr : entity work.rqkfag
    port map (lqjo => tcrvzegx);
end ukigh;



-- Seed after: 5864025884382836019,3108530264173481209
