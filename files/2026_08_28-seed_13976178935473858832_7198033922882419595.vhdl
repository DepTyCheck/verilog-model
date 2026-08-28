-- Seed: 13976178935473858832,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity lau is
  port (eryxfmyz : in std_logic_vector(2 downto 1); xlrke : buffer std_logic_vector(4 to 2));
end lau;

architecture jy of lau is
  
begin
  -- Multi-driven assignments
  xlrke <= (others => '0');
  xlrke <= "";
  xlrke <= xlrke;
  xlrke <= (others => '0');
end jy;

library ieee;
use ieee.std_logic_1164.all;

entity jepxctqow is
  port (qfi : in std_logic_vector(0 to 3));
end jepxctqow;

library ieee;
use ieee.std_logic_1164.all;

architecture gwouj of jepxctqow is
  signal bir : std_logic_vector(4 to 2);
  signal okbewxqv : std_logic_vector(2 downto 1);
  signal ltxbo : std_logic_vector(4 to 2);
  signal lrdh : std_logic_vector(2 downto 1);
begin
  cujferrvh : entity work.lau
    port map (eryxfmyz => lrdh, xlrke => ltxbo);
  bunupkry : entity work.lau
    port map (eryxfmyz => okbewxqv, xlrke => bir);
  
  -- Multi-driven assignments
  ltxbo <= "";
  okbewxqv <= lrdh;
  lrdh <= lrdh;
  lrdh <= okbewxqv;
end gwouj;



-- Seed after: 13929383825202409381,7198033922882419595
