-- Seed: 2675118800068961212,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity ffzh is
  port (ztyzdh : linkage std_logic; qjcallj : buffer time_vector(2 to 4); pm : inout time_vector(1 to 3));
end ffzh;

architecture np of ffzh is
  
begin
  -- Single-driven assignments
  pm <= (0_3_4.0_1_0_0 ps, 8#6_4_0_5_2.7043# ns, 1130.0140 ns);
  qjcallj <= (0 ps, 4_4 us, 431.31 us);
end np;

entity w is
  port (o : out real_vector(0 downto 4); lqx : out integer; nyfibdju : inout time);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture ogzcr of w is
  signal tkjrggwim : time_vector(1 to 3);
  signal lrvwsx : time_vector(2 to 4);
  signal bcgdpi : time_vector(1 to 3);
  signal vt : time_vector(2 to 4);
  signal nhkhmfkgg : time_vector(1 to 3);
  signal xvfoyldn : time_vector(2 to 4);
  signal qhears : std_logic;
begin
  jm : entity work.ffzh
    port map (ztyzdh => qhears, qjcallj => xvfoyldn, pm => nhkhmfkgg);
  dnj : entity work.ffzh
    port map (ztyzdh => qhears, qjcallj => vt, pm => bcgdpi);
  gh : entity work.ffzh
    port map (ztyzdh => qhears, qjcallj => lrvwsx, pm => tkjrggwim);
  
  -- Multi-driven assignments
  qhears <= '-';
  qhears <= 'L';
  qhears <= '0';
  qhears <= '1';
end ogzcr;

library ieee;
use ieee.std_logic_1164.all;

entity pvtsedkb is
  port (qclutg : linkage std_logic; frgfmlaar : in integer_vector(0 to 2); zjxqoyrjat : inout time);
end pvtsedkb;

architecture psrdxxikm of pvtsedkb is
  signal ayujlr : time_vector(1 to 3);
  signal fvxhpvt : time_vector(2 to 4);
  signal cj : time;
  signal ziu : integer;
  signal olwju : real_vector(0 downto 4);
begin
  asbnswqm : entity work.w
    port map (o => olwju, lqx => ziu, nyfibdju => cj);
  xf : entity work.ffzh
    port map (ztyzdh => qclutg, qjcallj => fvxhpvt, pm => ayujlr);
  
  -- Single-driven assignments
  zjxqoyrjat <= 0_3_3_0_2 fs;
end psrdxxikm;

entity dola is
  port (vrdmk : linkage real_vector(3 downto 0); nrox : buffer integer);
end dola;

architecture hafd of dola is
  
begin
  -- Single-driven assignments
  nrox <= 16#D_9_A#;
end hafd;



-- Seed after: 14742941836974517192,5472058987609252853
