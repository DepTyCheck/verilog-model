-- Seed: 16405210051530777291,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity yslxlfk is
  port (pgyycfmqlt : in boolean; c : in std_logic_vector(2 to 2); ycrtykzm : out std_logic; udyipzo : out time);
end yslxlfk;

architecture jdcnawiroa of yslxlfk is
  
begin
  -- Single-driven assignments
  udyipzo <= 1_0.3433 us;
  
  -- Multi-driven assignments
  ycrtykzm <= ycrtykzm;
  ycrtykzm <= ycrtykzm;
  ycrtykzm <= ycrtykzm;
end jdcnawiroa;

library ieee;
use ieee.std_logic_1164.all;

entity gxtcgbjni is
  port (bzxt : linkage std_logic_vector(4 downto 2));
end gxtcgbjni;

library ieee;
use ieee.std_logic_1164.all;

architecture dvpqxuud of gxtcgbjni is
  signal xvha : time;
  signal aeinxcz : std_logic;
  signal rpmvgpsan : boolean;
  signal ud : time;
  signal f : boolean;
  signal rrbg : time;
  signal jkmmpfkjwm : boolean;
  signal v : time;
  signal obfdguix : std_logic;
  signal qyvtcefsnf : std_logic_vector(2 to 2);
  signal k : boolean;
begin
  joblznxr : entity work.yslxlfk
    port map (pgyycfmqlt => k, c => qyvtcefsnf, ycrtykzm => obfdguix, udyipzo => v);
  iagacqp : entity work.yslxlfk
    port map (pgyycfmqlt => jkmmpfkjwm, c => qyvtcefsnf, ycrtykzm => obfdguix, udyipzo => rrbg);
  vlfxli : entity work.yslxlfk
    port map (pgyycfmqlt => f, c => qyvtcefsnf, ycrtykzm => obfdguix, udyipzo => ud);
  zsmbr : entity work.yslxlfk
    port map (pgyycfmqlt => rpmvgpsan, c => qyvtcefsnf, ycrtykzm => aeinxcz, udyipzo => xvha);
  
  -- Multi-driven assignments
  qyvtcefsnf <= (others => 'W');
  qyvtcefsnf <= (others => 'H');
  qyvtcefsnf <= "L";
end dvpqxuud;

library ieee;
use ieee.std_logic_1164.all;

entity vfamxcoqdb is
  port (towsofao : inout std_logic; snvvthgffc : in integer; xwezig : buffer bit_vector(1 to 4); xmqtnxcp : out boolean);
end vfamxcoqdb;

library ieee;
use ieee.std_logic_1164.all;

architecture md of vfamxcoqdb is
  signal ywvbacszk : std_logic_vector(4 downto 2);
  signal s : time;
  signal tld : std_logic;
  signal pjunjinr : std_logic_vector(2 to 2);
  signal wlgjnbxrgf : time;
  signal o : std_logic_vector(2 to 2);
  signal wghrrph : boolean;
begin
  n : entity work.yslxlfk
    port map (pgyycfmqlt => wghrrph, c => o, ycrtykzm => towsofao, udyipzo => wlgjnbxrgf);
  irquw : entity work.yslxlfk
    port map (pgyycfmqlt => xmqtnxcp, c => pjunjinr, ycrtykzm => tld, udyipzo => s);
  sfshsayty : entity work.gxtcgbjni
    port map (bzxt => ywvbacszk);
  
  -- Single-driven assignments
  xmqtnxcp <= TRUE;
  wghrrph <= FALSE;
  xwezig <= ('1', '1', '1', '1');
  
  -- Multi-driven assignments
  o <= o;
  pjunjinr <= o;
  pjunjinr <= o;
  towsofao <= towsofao;
end md;



-- Seed after: 14221048537381721368,2230106469645304029
