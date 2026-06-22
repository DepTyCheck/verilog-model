-- Seed: 16274398269188369846,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity nn is
  port (kaw : out string(5 to 4); kxcwa : inout std_logic_vector(4 to 2));
end nn;

architecture ofl of nn is
  
begin
  -- Single-driven assignments
  kaw <= "";
  
  -- Multi-driven assignments
  kxcwa <= (others => '0');
  kxcwa <= (others => '0');
  kxcwa <= "";
end ofl;

library ieee;
use ieee.std_logic_1164.all;

entity yyvcec is
  port (aylhalck : in std_logic_vector(4 downto 3); ycitblwqv : inout std_logic);
end yyvcec;

library ieee;
use ieee.std_logic_1164.all;

architecture sgq of yyvcec is
  signal kfagnm : std_logic_vector(4 to 2);
  signal rttbmf : string(5 to 4);
begin
  avxsvtf : entity work.nn
    port map (kaw => rttbmf, kxcwa => kfagnm);
  
  -- Multi-driven assignments
  ycitblwqv <= 'Z';
  ycitblwqv <= '1';
end sgq;



-- Seed after: 9953442154693712355,13479070923501788437
