-- Seed: 2317047584059817295,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity lu is
  port (jeyhude : out integer; svmzg : linkage boolean; uantn : in std_logic_vector(4 downto 4));
end lu;

architecture iuhwr of lu is
  
begin
  -- Single-driven assignments
  jeyhude <= 8#053#;
end iuhwr;

library ieee;
use ieee.std_logic_1164.all;

entity jzodlc is
  port (vfto : linkage integer; ozmohmckmy : linkage std_logic; eajjgsmhjv : buffer std_logic_vector(0 downto 0));
end jzodlc;

architecture f of jzodlc is
  
begin
  -- Multi-driven assignments
  eajjgsmhjv <= "X";
end f;

library ieee;
use ieee.std_logic_1164.all;

entity cyoc is
  port (my : out std_logic; hxsqppsk : buffer time_vector(4 to 4); ku : out std_logic_vector(4 downto 2); fpjttkb : inout time);
end cyoc;

library ieee;
use ieee.std_logic_1164.all;

architecture ccylf of cyoc is
  signal y : std_logic_vector(0 downto 0);
  signal kltzazaa : integer;
  signal zmnec : boolean;
  signal e : integer;
  signal bl : std_logic_vector(4 downto 4);
  signal nqmex : boolean;
  signal oan : integer;
  signal wm : std_logic_vector(4 downto 4);
  signal pdhs : boolean;
  signal pirwtoa : integer;
begin
  altegu : entity work.lu
    port map (jeyhude => pirwtoa, svmzg => pdhs, uantn => wm);
  sygl : entity work.lu
    port map (jeyhude => oan, svmzg => nqmex, uantn => bl);
  t : entity work.lu
    port map (jeyhude => e, svmzg => zmnec, uantn => wm);
  nxvn : entity work.jzodlc
    port map (vfto => kltzazaa, ozmohmckmy => my, eajjgsmhjv => y);
  
  -- Single-driven assignments
  fpjttkb <= 1 hr;
  hxsqppsk <= (others => 3_1_1_2_2 fs);
  
  -- Multi-driven assignments
  wm <= "-";
  ku <= ('W', 'X', '-');
  ku <= "UZ1";
  y <= (others => '0');
end ccylf;

entity qcrwjs is
  port (alcasy : linkage time);
end qcrwjs;

library ieee;
use ieee.std_logic_1164.all;

architecture qjpkaulpz of qcrwjs is
  signal lbj : integer;
  signal rfynrzdxw : time;
  signal wg : std_logic_vector(4 downto 2);
  signal zhniorm : time_vector(4 to 4);
  signal wn : std_logic;
  signal veogfmwem : std_logic_vector(4 downto 4);
  signal x : boolean;
  signal ujkeaxx : integer;
  signal h : std_logic_vector(0 downto 0);
  signal nmfecooduh : boolean;
  signal aqwftivfo : integer;
begin
  tfavgvp : entity work.lu
    port map (jeyhude => aqwftivfo, svmzg => nmfecooduh, uantn => h);
  nroautf : entity work.lu
    port map (jeyhude => ujkeaxx, svmzg => x, uantn => veogfmwem);
  fqsqn : entity work.cyoc
    port map (my => wn, hxsqppsk => zhniorm, ku => wg, fpjttkb => rfynrzdxw);
  cbpwbyh : entity work.jzodlc
    port map (vfto => lbj, ozmohmckmy => wn, eajjgsmhjv => h);
  
  -- Multi-driven assignments
  wn <= '0';
  h <= (others => 'Z');
  h <= "L";
end qjpkaulpz;



-- Seed after: 12506770235387997200,10557070023141912087
