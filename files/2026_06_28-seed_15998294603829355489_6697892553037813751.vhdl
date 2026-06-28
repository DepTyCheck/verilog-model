-- Seed: 15998294603829355489,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity gtyvxgj is
  port (fqb : inout std_logic_vector(4 to 1); rdhvjusez : buffer std_logic_vector(3 downto 4));
end gtyvxgj;

architecture rfsmxhsx of gtyvxgj is
  
begin
  -- Multi-driven assignments
  rdhvjusez <= "";
  rdhvjusez <= "";
end rfsmxhsx;

entity d is
  port (cixjrmtud : buffer bit_vector(4 downto 0); z : in time; j : in bit_vector(0 to 4));
end d;

library ieee;
use ieee.std_logic_1164.all;

architecture lkeynmu of d is
  signal fkxzlnzbyq : std_logic_vector(3 downto 4);
  signal wtqpawg : std_logic_vector(4 to 1);
  signal o : std_logic_vector(3 downto 4);
  signal lzzqdvepch : std_logic_vector(4 to 1);
begin
  lkzprks : entity work.gtyvxgj
    port map (fqb => lzzqdvepch, rdhvjusez => lzzqdvepch);
  e : entity work.gtyvxgj
    port map (fqb => lzzqdvepch, rdhvjusez => o);
  sjntphnmo : entity work.gtyvxgj
    port map (fqb => wtqpawg, rdhvjusez => fkxzlnzbyq);
  
  -- Single-driven assignments
  cixjrmtud <= ('1', '1', '0', '0', '0');
end lkeynmu;



-- Seed after: 4030493153136594261,6697892553037813751
