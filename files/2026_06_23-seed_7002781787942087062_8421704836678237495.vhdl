-- Seed: 7002781787942087062,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity aggnc is
  port (urw : inout real; fgnxb : out std_logic_vector(3 to 4); ditzokk : buffer time_vector(2 downto 2); lee : out time);
end aggnc;

architecture yrakqec of aggnc is
  
begin
  -- Multi-driven assignments
  fgnxb <= ('W', '0');
  fgnxb <= ('H', '-');
  fgnxb <= "HU";
  fgnxb <= ('Z', 'H');
end yrakqec;

entity sjp is
  port (vpa : buffer time);
end sjp;

architecture rqkrb of sjp is
  
begin
  -- Single-driven assignments
  vpa <= 8#7_5_7_6.3# fs;
end rqkrb;

library ieee;
use ieee.std_logic_1164.all;

entity bfhkxaqmy is
  port (amzu : linkage real; xn : in std_logic; exbusf : linkage string(1 to 4); gltvct : inout integer);
end bfhkxaqmy;

architecture aeuh of bfhkxaqmy is
  
begin
  
end aeuh;

library ieee;
use ieee.std_logic_1164.all;

entity nh is
  port (radx : linkage character; wmfwqfhx : buffer std_logic_vector(0 downto 2); z : in real_vector(4 downto 0); wseratx : inout severity_level);
end nh;

library ieee;
use ieee.std_logic_1164.all;

architecture qw of nh is
  signal ppekdz : integer;
  signal v : string(1 to 4);
  signal whxnfqyr : std_logic;
  signal oyehe : real;
  signal gxjrk : time;
  signal wlsp : time_vector(2 downto 2);
  signal ppaut : real;
  signal k : time;
  signal ydic : time_vector(2 downto 2);
  signal wywryaknq : std_logic_vector(3 to 4);
  signal tlcxhrhjsd : real;
begin
  edfice : entity work.aggnc
    port map (urw => tlcxhrhjsd, fgnxb => wywryaknq, ditzokk => ydic, lee => k);
  saqwe : entity work.aggnc
    port map (urw => ppaut, fgnxb => wywryaknq, ditzokk => wlsp, lee => gxjrk);
  lvwljliwti : entity work.bfhkxaqmy
    port map (amzu => oyehe, xn => whxnfqyr, exbusf => v, gltvct => ppekdz);
end qw;



-- Seed after: 13481041168319354246,8421704836678237495
