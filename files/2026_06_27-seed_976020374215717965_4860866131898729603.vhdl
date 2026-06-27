-- Seed: 976020374215717965,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity eoykpz is
  port (ilz : linkage integer; n : in std_logic; zlxed : in std_logic_vector(1 to 4); ljvqjas : linkage std_logic);
end eoykpz;

architecture efivgps of eoykpz is
  
begin
  
end efivgps;

library ieee;
use ieee.std_logic_1164.all;

entity mncpjqscky is
  port (utpn : in std_logic; onpupyxj : linkage std_logic; pxemz : out time_vector(4 to 3); hfo : linkage real);
end mncpjqscky;

library ieee;
use ieee.std_logic_1164.all;

architecture zefrdfxon of mncpjqscky is
  signal vzc : std_logic;
  signal hihsdyxe : integer;
  signal bwdbalekib : std_logic_vector(1 to 4);
  signal xaiptlds : integer;
begin
  qkej : entity work.eoykpz
    port map (ilz => xaiptlds, n => utpn, zlxed => bwdbalekib, ljvqjas => onpupyxj);
  kizxzix : entity work.eoykpz
    port map (ilz => hihsdyxe, n => utpn, zlxed => bwdbalekib, ljvqjas => vzc);
  
  -- Single-driven assignments
  pxemz <= (others => 0 ns);
  
  -- Multi-driven assignments
  vzc <= 'X';
  vzc <= 'W';
end zefrdfxon;

entity vlyojqor is
  port (n : out time; xywocj : inout integer; kbn : buffer bit; qidohb : buffer bit_vector(0 to 0));
end vlyojqor;

architecture ww of vlyojqor is
  
begin
  -- Single-driven assignments
  qidohb <= (others => '0');
  n <= 8#2# us;
  xywocj <= 16#C_F_6#;
end ww;

library ieee;
use ieee.std_logic_1164.all;

entity qiueyfr is
  port (gbdkb : buffer integer; logg : inout integer; bg : inout time; knrmpuqx : buffer std_logic_vector(4 downto 3));
end qiueyfr;

library ieee;
use ieee.std_logic_1164.all;

architecture cmcwjydui of qiueyfr is
  signal cqzijotlr : real;
  signal kjdnsc : time_vector(4 to 3);
  signal wgypawz : real;
  signal eqasawa : time_vector(4 to 3);
  signal v : std_logic;
  signal bqs : std_logic_vector(1 to 4);
  signal wzrokoseox : std_logic;
  signal dmjwghabog : integer;
  signal cwqdxqudwt : bit_vector(0 to 0);
  signal bbge : bit;
begin
  iqcapbphb : entity work.vlyojqor
    port map (n => bg, xywocj => gbdkb, kbn => bbge, qidohb => cwqdxqudwt);
  rhsz : entity work.eoykpz
    port map (ilz => dmjwghabog, n => wzrokoseox, zlxed => bqs, ljvqjas => wzrokoseox);
  qpacz : entity work.mncpjqscky
    port map (utpn => wzrokoseox, onpupyxj => v, pxemz => eqasawa, hfo => wgypawz);
  ujciydvdz : entity work.mncpjqscky
    port map (utpn => wzrokoseox, onpupyxj => wzrokoseox, pxemz => kjdnsc, hfo => cqzijotlr);
end cmcwjydui;



-- Seed after: 3058893733650398404,4860866131898729603
