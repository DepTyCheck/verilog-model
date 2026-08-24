-- Seed: 3651940653521690246,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity vhp is
  port (fakxoowpqb : in std_logic_vector(3 to 3); dxztfc : in std_logic_vector(2 to 2); onfgnyglal : in integer);
end vhp;

architecture zttsjc of vhp is
  
begin
  
end zttsjc;

entity nkqpckzk is
  port (pzupud : inout time; qhlw : out time; iv : buffer integer_vector(0 to 4));
end nkqpckzk;

library ieee;
use ieee.std_logic_1164.all;

architecture an of nkqpckzk is
  signal svji : integer;
  signal mrlpsw : integer;
  signal dlxionfg : std_logic_vector(2 to 2);
begin
  ziqmwvifn : entity work.vhp
    port map (fakxoowpqb => dlxionfg, dxztfc => dlxionfg, onfgnyglal => mrlpsw);
  gfntasjffr : entity work.vhp
    port map (fakxoowpqb => dlxionfg, dxztfc => dlxionfg, onfgnyglal => svji);
  
  -- Single-driven assignments
  iv <= (8#1#, 0, 8#2210#, 16#E#, 1114);
  
  -- Multi-driven assignments
  dlxionfg <= "Z";
  dlxionfg <= (others => 'U');
  dlxionfg <= dlxionfg;
end an;



-- Seed after: 7205155507955218660,16159265764638711791
