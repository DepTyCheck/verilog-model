-- Seed: 15473052133420635632,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity qnbbs is
  port (lg : in time_vector(0 to 4); asbb : buffer std_logic; zqshcomi : in std_logic; ngdtdgjvw : linkage real_vector(0 downto 4));
end qnbbs;

architecture htnqm of qnbbs is
  
begin
  -- Multi-driven assignments
  asbb <= zqshcomi;
  asbb <= 'H';
  asbb <= 'H';
  asbb <= 'U';
end htnqm;

library ieee;
use ieee.std_logic_1164.all;

entity qdbbxdp is
  port (jxohpppumt : in std_logic; jhuhtry : in integer; kuslqorwrg : in time);
end qdbbxdp;

library ieee;
use ieee.std_logic_1164.all;

architecture gccmb of qdbbxdp is
  signal oh : real_vector(0 downto 4);
  signal xnvp : std_logic;
  signal qkzugeojl : real_vector(0 downto 4);
  signal fcjckbuyo : std_logic;
  signal jtfmp : time_vector(0 to 4);
  signal xjhupnk : real_vector(0 downto 4);
  signal sxkt : std_logic;
  signal wp : time_vector(0 to 4);
  signal emtyuvoao : real_vector(0 downto 4);
  signal jvfvktfxa : std_logic;
  signal xrclfjxi : time_vector(0 to 4);
begin
  qeuci : entity work.qnbbs
    port map (lg => xrclfjxi, asbb => jvfvktfxa, zqshcomi => jxohpppumt, ngdtdgjvw => emtyuvoao);
  ntoi : entity work.qnbbs
    port map (lg => wp, asbb => sxkt, zqshcomi => jvfvktfxa, ngdtdgjvw => xjhupnk);
  uwwzg : entity work.qnbbs
    port map (lg => jtfmp, asbb => jvfvktfxa, zqshcomi => fcjckbuyo, ngdtdgjvw => qkzugeojl);
  xhhvssdtpz : entity work.qnbbs
    port map (lg => xrclfjxi, asbb => jvfvktfxa, zqshcomi => xnvp, ngdtdgjvw => oh);
  
  -- Multi-driven assignments
  jvfvktfxa <= jxohpppumt;
end gccmb;

entity tqf is
  port (vicd : in integer; jxkpundqvm : out time; wgpoarirjh : out time_vector(4 downto 3));
end tqf;

library ieee;
use ieee.std_logic_1164.all;

architecture suydxqjgx of tqf is
  signal ivghzf : std_logic;
  signal jvq : real_vector(0 downto 4);
  signal yyw : std_logic;
  signal o : std_logic;
  signal g : time_vector(0 to 4);
begin
  cvfz : entity work.qnbbs
    port map (lg => g, asbb => o, zqshcomi => yyw, ngdtdgjvw => jvq);
  zugpicznq : entity work.qdbbxdp
    port map (jxohpppumt => ivghzf, jhuhtry => vicd, kuslqorwrg => jxkpundqvm);
  
  -- Single-driven assignments
  jxkpundqvm <= 3 us;
  
  -- Multi-driven assignments
  o <= 'Z';
end suydxqjgx;

entity zlbtk is
  port (tk : inout integer);
end zlbtk;

library ieee;
use ieee.std_logic_1164.all;

architecture jmsrd of zlbtk is
  signal cpjot : real_vector(0 downto 4);
  signal ujfeyjk : std_logic;
  signal xx : time_vector(0 to 4);
  signal jeoa : real_vector(0 downto 4);
  signal zvltwgk : time_vector(0 to 4);
  signal ucfykh : real_vector(0 downto 4);
  signal gwemwsdwg : std_logic;
  signal ycapyqmztw : std_logic;
  signal hywshowda : real_vector(0 downto 4);
  signal fslecacag : std_logic;
  signal tqg : std_logic;
  signal tfgjhsae : time_vector(0 to 4);
begin
  svvynqqeby : entity work.qnbbs
    port map (lg => tfgjhsae, asbb => tqg, zqshcomi => fslecacag, ngdtdgjvw => hywshowda);
  aduh : entity work.qnbbs
    port map (lg => tfgjhsae, asbb => ycapyqmztw, zqshcomi => gwemwsdwg, ngdtdgjvw => ucfykh);
  lfavp : entity work.qnbbs
    port map (lg => zvltwgk, asbb => fslecacag, zqshcomi => tqg, ngdtdgjvw => jeoa);
  zihlokc : entity work.qnbbs
    port map (lg => xx, asbb => ujfeyjk, zqshcomi => gwemwsdwg, ngdtdgjvw => cpjot);
  
  -- Single-driven assignments
  zvltwgk <= tfgjhsae;
  tk <= 1;
  
  -- Multi-driven assignments
  ycapyqmztw <= tqg;
  tqg <= tqg;
end jmsrd;



-- Seed after: 12575845558950116812,12359743974512393525
