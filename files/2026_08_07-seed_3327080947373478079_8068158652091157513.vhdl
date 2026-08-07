-- Seed: 3327080947373478079,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity tzjlrtuwx is
  port (pku : linkage bit; ikqdquln : out std_logic; reydidr : inout boolean_vector(3 downto 3); x : out real_vector(4 downto 2));
end tzjlrtuwx;

architecture oflepnyjh of tzjlrtuwx is
  
begin
  -- Single-driven assignments
  x <= (8#5_4_6_1_6.3#, 2.20, 16#52D.D#);
  
  -- Multi-driven assignments
  ikqdquln <= 'W';
end oflepnyjh;

library ieee;
use ieee.std_logic_1164.all;

entity lxqxactp is
  port (ogqyyv : buffer time; vvaa : in integer; abvv : in std_logic_vector(0 downto 3));
end lxqxactp;

architecture bijlcx of lxqxactp is
  
begin
  -- Single-driven assignments
  ogqyyv <= ogqyyv;
end bijlcx;

entity kgeymsoh is
  port (kmfnati : inout time);
end kgeymsoh;

library ieee;
use ieee.std_logic_1164.all;

architecture lgibcbeeg of kgeymsoh is
  signal otywnnf : real_vector(4 downto 2);
  signal imraujv : boolean_vector(3 downto 3);
  signal j : std_logic;
  signal o : bit;
begin
  inwkl : entity work.tzjlrtuwx
    port map (pku => o, ikqdquln => j, reydidr => imraujv, x => otywnnf);
  
  -- Single-driven assignments
  kmfnati <= 4 sec;
  
  -- Multi-driven assignments
  j <= '0';
  j <= 'X';
end lgibcbeeg;

entity c is
  port (myp : in real);
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture vul of c is
  signal jbstyiwjik : real_vector(4 downto 2);
  signal evfagifnxk : boolean_vector(3 downto 3);
  signal pwzexkmfrm : bit;
  signal flmcpr : time;
  signal nh : real_vector(4 downto 2);
  signal sjkrxqa : boolean_vector(3 downto 3);
  signal mv : std_logic;
  signal vyijiw : bit;
  signal dbmg : std_logic_vector(0 downto 3);
  signal d : integer;
  signal wjpwucgag : time;
begin
  uhhoxdyiuh : entity work.lxqxactp
    port map (ogqyyv => wjpwucgag, vvaa => d, abvv => dbmg);
  ngsbfyvfoe : entity work.tzjlrtuwx
    port map (pku => vyijiw, ikqdquln => mv, reydidr => sjkrxqa, x => nh);
  nffg : entity work.kgeymsoh
    port map (kmfnati => flmcpr);
  ibe : entity work.tzjlrtuwx
    port map (pku => pwzexkmfrm, ikqdquln => mv, reydidr => evfagifnxk, x => jbstyiwjik);
  
  -- Multi-driven assignments
  dbmg <= (others => '0');
  mv <= 'H';
  dbmg <= (others => '0');
end vul;



-- Seed after: 9307054825755789525,8068158652091157513
