-- Seed: 8909703465098734747,8118127366649987907

entity psmcuidexj is
  port (krz : in time; qxj : out real);
end psmcuidexj;

architecture xr of psmcuidexj is
  
begin
  -- Single-driven assignments
  qxj <= 3_1_1.00103;
end xr;

library ieee;
use ieee.std_logic_1164.all;

entity sxa is
  port (xbhyme : in std_logic);
end sxa;

architecture ncvavea of sxa is
  signal vgieyi : real;
  signal orzg : time;
  signal d : real;
  signal hrvwqzjm : real;
  signal yrzo : real;
  signal ksmmsjs : time;
begin
  siosim : entity work.psmcuidexj
    port map (krz => ksmmsjs, qxj => yrzo);
  kkrijx : entity work.psmcuidexj
    port map (krz => ksmmsjs, qxj => hrvwqzjm);
  wexisbhkb : entity work.psmcuidexj
    port map (krz => ksmmsjs, qxj => d);
  dlqbmyxnyh : entity work.psmcuidexj
    port map (krz => orzg, qxj => vgieyi);
  
  -- Single-driven assignments
  ksmmsjs <= 4 min;
  orzg <= 1 min;
end ncvavea;

entity angfnvpwb is
  port (gvvh : out real; qt : out integer; aekpf : linkage real; l : out integer);
end angfnvpwb;

architecture uawclub of angfnvpwb is
  signal bjssa : real;
  signal drwjnk : time;
begin
  axnmc : entity work.psmcuidexj
    port map (krz => drwjnk, qxj => bjssa);
  
  -- Single-driven assignments
  l <= 8#2_5_7_1_6#;
  gvvh <= 3_0_1_1_3.1;
  qt <= 16#AE2D0#;
end uawclub;



-- Seed after: 8764602367612899016,8118127366649987907
