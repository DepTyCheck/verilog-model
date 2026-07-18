-- Seed: 15875271208002142698,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (tyeyqzi : out integer; cgogflyuky : out std_logic_vector(1 to 1); sudi : inout integer);
end m;

architecture mdv of m is
  
begin
  
end mdv;

entity nawdtea is
  port (kcu : in time; pmuav : buffer time; wzdfhs : in time_vector(4 to 4); jhb : buffer boolean_vector(4 to 0));
end nawdtea;

library ieee;
use ieee.std_logic_1164.all;

architecture pk of nawdtea is
  signal tlyilqyy : integer;
  signal myigfcz : std_logic_vector(1 to 1);
  signal duplj : integer;
  signal tgjcuro : integer;
  signal rx : integer;
  signal zkyunxjzh : integer;
  signal cymfxlt : std_logic_vector(1 to 1);
  signal ehwuwb : integer;
begin
  nknyofbxsb : entity work.m
    port map (tyeyqzi => ehwuwb, cgogflyuky => cymfxlt, sudi => zkyunxjzh);
  xdt : entity work.m
    port map (tyeyqzi => rx, cgogflyuky => cymfxlt, sudi => tgjcuro);
  zgywklntqn : entity work.m
    port map (tyeyqzi => duplj, cgogflyuky => myigfcz, sudi => tlyilqyy);
  
  -- Single-driven assignments
  jhb <= (others => TRUE);
  
  -- Multi-driven assignments
  cymfxlt <= (others => 'H');
  myigfcz <= cymfxlt;
  cymfxlt <= "U";
  myigfcz <= "L";
end pk;



-- Seed after: 154067107821107330,1112937151005418631
