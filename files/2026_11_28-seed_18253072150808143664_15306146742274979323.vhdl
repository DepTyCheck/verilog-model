-- Seed: 18253072150808143664,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (syoalx : in time_vector(2 to 4); yc : in integer; ioo : in std_logic_vector(2 to 2); brukt : inout integer_vector(3 to 0));
end i;

architecture gcmqnfch of i is
  
begin
  -- Single-driven assignments
  brukt <= brukt;
end gcmqnfch;

library ieee;
use ieee.std_logic_1164.all;

entity nykph is
  port (l : buffer time; xqrkv : out std_logic);
end nykph;

library ieee;
use ieee.std_logic_1164.all;

architecture bvoda of nykph is
  signal bzhjbrgdsd : integer_vector(3 to 0);
  signal mmldyextxf : std_logic_vector(2 to 2);
  signal wtyj : integer_vector(3 to 0);
  signal bhqjduba : std_logic_vector(2 to 2);
  signal smezeor : time_vector(2 to 4);
  signal law : integer_vector(3 to 0);
  signal iuj : std_logic_vector(2 to 2);
  signal zsc : integer;
  signal xqwkh : time_vector(2 to 4);
begin
  paqzcm : entity work.i
    port map (syoalx => xqwkh, yc => zsc, ioo => iuj, brukt => law);
  poi : entity work.i
    port map (syoalx => smezeor, yc => zsc, ioo => bhqjduba, brukt => wtyj);
  hdurswajjl : entity work.i
    port map (syoalx => xqwkh, yc => zsc, ioo => mmldyextxf, brukt => bzhjbrgdsd);
  
  -- Multi-driven assignments
  iuj <= (others => 'Z');
  xqrkv <= 'H';
  xqrkv <= 'U';
  iuj <= iuj;
end bvoda;

entity z is
  port (goelboaqb : inout string(5 to 2));
end z;

library ieee;
use ieee.std_logic_1164.all;

architecture auxnedyw of z is
  signal q : time;
  signal buoiwupys : std_logic;
  signal dxvf : time;
  signal ijexhng : integer_vector(3 to 0);
  signal srtlplwtyh : std_logic_vector(2 to 2);
  signal iafaxjzs : integer;
  signal arpkudjxl : time_vector(2 to 4);
begin
  nw : entity work.i
    port map (syoalx => arpkudjxl, yc => iafaxjzs, ioo => srtlplwtyh, brukt => ijexhng);
  sxcqdbxyn : entity work.nykph
    port map (l => dxvf, xqrkv => buoiwupys);
  ngov : entity work.nykph
    port map (l => q, xqrkv => buoiwupys);
  
  -- Single-driven assignments
  goelboaqb <= goelboaqb;
  iafaxjzs <= iafaxjzs;
  arpkudjxl <= arpkudjxl;
  
  -- Multi-driven assignments
  buoiwupys <= '1';
  srtlplwtyh <= "U";
end auxnedyw;



-- Seed after: 12361591299048812194,15306146742274979323
