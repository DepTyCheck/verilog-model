-- Seed: 18357471241021253824,4292249356257567981

entity miepqkusr is
  port (p : buffer integer; d : linkage time; giui : out boolean_vector(3 to 3));
end miepqkusr;

architecture uzzt of miepqkusr is
  
begin
  -- Single-driven assignments
  giui <= giui;
  p <= 8#2_0#;
end uzzt;

entity g is
  port (mlvjx : buffer time_vector(0 to 3); vqobewca : in time_vector(1 to 3); bn : buffer character; oduziag : in integer);
end g;

architecture neark of g is
  signal faei : boolean_vector(3 to 3);
  signal pdrvbrlsje : time;
  signal r : integer;
begin
  bryg : entity work.miepqkusr
    port map (p => r, d => pdrvbrlsje, giui => faei);
  
  -- Single-driven assignments
  mlvjx <= mlvjx;
  bn <= bn;
end neark;

library ieee;
use ieee.std_logic_1164.all;

entity wfew is
  port (jozqn : out std_logic);
end wfew;

architecture arbqvzs of wfew is
  signal yeaebdxz : character;
  signal ygr : time_vector(0 to 3);
  signal dcj : integer;
  signal qvlfvxlgz : character;
  signal rg : time_vector(1 to 3);
  signal t : time_vector(0 to 3);
begin
  q : entity work.g
    port map (mlvjx => t, vqobewca => rg, bn => qvlfvxlgz, oduziag => dcj);
  rukds : entity work.g
    port map (mlvjx => ygr, vqobewca => rg, bn => yeaebdxz, oduziag => dcj);
  
  -- Single-driven assignments
  rg <= rg;
  dcj <= 2#1_0#;
  
  -- Multi-driven assignments
  jozqn <= jozqn;
  jozqn <= jozqn;
  jozqn <= 'W';
  jozqn <= jozqn;
end arbqvzs;



-- Seed after: 14732197219434329071,4292249356257567981
