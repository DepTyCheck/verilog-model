-- Seed: 11719071592125453123,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity hmde is
  port (ndkhct : out bit_vector(4 downto 2); frpcu : buffer integer; qsiwexvee : out std_logic_vector(4 downto 3));
end hmde;

architecture zl of hmde is
  
begin
  -- Multi-driven assignments
  qsiwexvee <= ('-', 'Z');
end zl;

library ieee;
use ieee.std_logic_1164.all;

entity fm is
  port (prjju : out std_logic_vector(4 to 3));
end fm;

library ieee;
use ieee.std_logic_1164.all;

architecture ycldfyy of fm is
  signal rlr : std_logic_vector(4 downto 3);
  signal q : integer;
  signal ramoaeoh : bit_vector(4 downto 2);
begin
  xndyuj : entity work.hmde
    port map (ndkhct => ramoaeoh, frpcu => q, qsiwexvee => rlr);
  
  -- Multi-driven assignments
  prjju <= "";
  rlr <= rlr;
  prjju <= (others => '0');
end ycldfyy;



-- Seed after: 10182271543957226021,12359743974512393525
