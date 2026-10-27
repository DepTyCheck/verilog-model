-- Seed: 11452535730377920516,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity ohs is
  port (owkqdbj : out std_logic_vector(0 downto 2); msgjwphkv : buffer time);
end ohs;

architecture lrxwyox of ohs is
  
begin
  -- Single-driven assignments
  msgjwphkv <= msgjwphkv;
  
  -- Multi-driven assignments
  owkqdbj <= "";
  owkqdbj <= owkqdbj;
  owkqdbj <= owkqdbj;
  owkqdbj <= (others => '0');
end lrxwyox;

entity sri is
  port (bybtlcp : inout time; d : in real);
end sri;

library ieee;
use ieee.std_logic_1164.all;

architecture athz of sri is
  signal sgmhwaq : std_logic_vector(0 downto 2);
  signal bmg : time;
  signal fkjbmnkmhg : std_logic_vector(0 downto 2);
begin
  ifpbwdkzqu : entity work.ohs
    port map (owkqdbj => fkjbmnkmhg, msgjwphkv => bmg);
  efoogrw : entity work.ohs
    port map (owkqdbj => sgmhwaq, msgjwphkv => bybtlcp);
  
  -- Multi-driven assignments
  fkjbmnkmhg <= "";
  fkjbmnkmhg <= fkjbmnkmhg;
  fkjbmnkmhg <= fkjbmnkmhg;
end athz;



-- Seed after: 11689837829336348439,12536735141187299017
