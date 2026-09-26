-- Seed: 2541835440909658673,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity yadec is
  port (byyyes : linkage std_logic; bvzkah : in string(3 downto 4));
end yadec;

architecture srnfht of yadec is
  
begin
  
end srnfht;

library ieee;
use ieee.std_logic_1164.all;

entity wmrxcnk is
  port (wekjrcs : linkage std_logic; vblxdj : in boolean);
end wmrxcnk;

library ieee;
use ieee.std_logic_1164.all;

architecture mvz of wmrxcnk is
  signal fpwkffetb : string(3 downto 4);
  signal mjvbnvdtf : std_logic;
  signal oxdzdc : string(3 downto 4);
  signal wpssdprldr : std_logic;
  signal kqu : string(3 downto 4);
  signal auwudy : string(3 downto 4);
  signal oypodfflk : std_logic;
begin
  smiowksfnf : entity work.yadec
    port map (byyyes => oypodfflk, bvzkah => auwudy);
  tzxupo : entity work.yadec
    port map (byyyes => wekjrcs, bvzkah => kqu);
  rhgoccslfo : entity work.yadec
    port map (byyyes => wpssdprldr, bvzkah => oxdzdc);
  zqixywgx : entity work.yadec
    port map (byyyes => mjvbnvdtf, bvzkah => fpwkffetb);
  
  -- Single-driven assignments
  auwudy <= (others => ' ');
  oxdzdc <= auwudy;
  
  -- Multi-driven assignments
  mjvbnvdtf <= wpssdprldr;
  oypodfflk <= '1';
  oypodfflk <= 'W';
end mvz;



-- Seed after: 15591583624221775059,10875537289884587119
