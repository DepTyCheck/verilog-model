-- Seed: 2841915539490686097,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity diklmu is
  port (acojthn : buffer std_logic_vector(3 downto 2));
end diklmu;

architecture wxbef of diklmu is
  
begin
  
end wxbef;

entity ysnhjhjp is
  port (fsfzqrpr : inout integer; wgyah : buffer time);
end ysnhjhjp;

library ieee;
use ieee.std_logic_1164.all;

architecture mzpjmibut of ysnhjhjp is
  signal xjdljabp : std_logic_vector(3 downto 2);
  signal tdeat : std_logic_vector(3 downto 2);
  signal ejagvsfxk : std_logic_vector(3 downto 2);
begin
  ginsuaixh : entity work.diklmu
    port map (acojthn => ejagvsfxk);
  yjygwgpj : entity work.diklmu
    port map (acojthn => tdeat);
  ykc : entity work.diklmu
    port map (acojthn => ejagvsfxk);
  yjbtfibd : entity work.diklmu
    port map (acojthn => xjdljabp);
  
  -- Multi-driven assignments
  ejagvsfxk <= ('-', 'H');
  ejagvsfxk <= "10";
end mzpjmibut;



-- Seed after: 8536765567610514595,4122021602305298647
