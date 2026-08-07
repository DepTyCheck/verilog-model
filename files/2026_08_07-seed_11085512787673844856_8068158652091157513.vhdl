-- Seed: 11085512787673844856,8068158652091157513

entity ott is
  port (yyanqlyh : in boolean_vector(1 to 3); qwxkuj : in real; cht : out real);
end ott;

architecture nqflpbsyk of ott is
  
begin
  -- Single-driven assignments
  cht <= cht;
end nqflpbsyk;

entity yqd is
  port (felx : buffer real; fohyud : in time);
end yqd;

architecture gfkjwutrxu of yqd is
  signal vuib : real;
  signal ptdnwldl : real;
  signal jyvhsbas : boolean_vector(1 to 3);
begin
  fdgx : entity work.ott
    port map (yyanqlyh => jyvhsbas, qwxkuj => ptdnwldl, cht => felx);
  lyjnrn : entity work.ott
    port map (yyanqlyh => jyvhsbas, qwxkuj => vuib, cht => vuib);
  
  -- Single-driven assignments
  ptdnwldl <= 8#4.12#;
  jyvhsbas <= (TRUE, TRUE, TRUE);
end gfkjwutrxu;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (siggcm : in std_logic; ubbmnvfu : buffer real; edayojcwwl : buffer std_logic);
end r;

architecture eztxsrdkk of r is
  signal rkxwdhm : time;
begin
  xv : entity work.yqd
    port map (felx => ubbmnvfu, fohyud => rkxwdhm);
  
  -- Single-driven assignments
  rkxwdhm <= rkxwdhm;
end eztxsrdkk;



-- Seed after: 7164458603053184703,8068158652091157513
