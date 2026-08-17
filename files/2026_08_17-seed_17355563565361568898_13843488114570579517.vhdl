-- Seed: 17355563565361568898,13843488114570579517

entity tj is
  port (lwhiqf : in boolean_vector(3 to 3));
end tj;

architecture ulcpq of tj is
  
begin
  
end ulcpq;

library ieee;
use ieee.std_logic_1164.all;

entity mkdxgeaszi is
  port (brlrqkhe : buffer std_logic_vector(1 to 2));
end mkdxgeaszi;

architecture zkv of mkdxgeaszi is
  signal rljqwagys : boolean_vector(3 to 3);
  signal yqppcayypt : boolean_vector(3 to 3);
begin
  jr : entity work.tj
    port map (lwhiqf => yqppcayypt);
  plap : entity work.tj
    port map (lwhiqf => rljqwagys);
  
  -- Single-driven assignments
  yqppcayypt <= yqppcayypt;
  rljqwagys <= (others => FALSE);
  
  -- Multi-driven assignments
  brlrqkhe <= brlrqkhe;
  brlrqkhe <= brlrqkhe;
  brlrqkhe <= "ZH";
  brlrqkhe <= brlrqkhe;
end zkv;

library ieee;
use ieee.std_logic_1164.all;

entity zndyl is
  port (acmlcuoe : out std_logic; k : inout boolean_vector(2 to 3); lgt : out integer);
end zndyl;

library ieee;
use ieee.std_logic_1164.all;

architecture wob of zndyl is
  signal ub : boolean_vector(3 to 3);
  signal qrs : std_logic_vector(1 to 2);
  signal m : boolean_vector(3 to 3);
begin
  tgxmvafwuj : entity work.tj
    port map (lwhiqf => m);
  ujfdsmerq : entity work.mkdxgeaszi
    port map (brlrqkhe => qrs);
  nqn : entity work.tj
    port map (lwhiqf => ub);
  h : entity work.mkdxgeaszi
    port map (brlrqkhe => qrs);
  
  -- Single-driven assignments
  m <= (others => FALSE);
  
  -- Multi-driven assignments
  acmlcuoe <= 'W';
  acmlcuoe <= acmlcuoe;
end wob;



-- Seed after: 4823615998472314409,13843488114570579517
