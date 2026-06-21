-- Seed: 7599067087390089403,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity tmyhs is
  port (zbrq : out std_logic_vector(0 to 2); onrmrf : in integer; cjris : out severity_level; mctoioikzb : linkage std_logic);
end tmyhs;

architecture r of tmyhs is
  
begin
  -- Multi-driven assignments
  zbrq <= ('0', 'W', '0');
  zbrq <= ('-', 'Z', 'X');
end r;

library ieee;
use ieee.std_logic_1164.all;

entity ndeahwzj is
  port (rueqtyvfc : inout std_logic; vuetanbj : linkage std_logic_vector(0 to 3); bhmuhmw : inout time);
end ndeahwzj;

library ieee;
use ieee.std_logic_1164.all;

architecture keklswzs of ndeahwzj is
  signal vqqbjnni : severity_level;
  signal gknr : integer;
  signal ortrkxi : std_logic_vector(0 to 2);
  signal ebhbxlki : std_logic;
  signal zaybxb : severity_level;
  signal xsuwxtvsxc : std_logic_vector(0 to 2);
  signal stgsc : severity_level;
  signal n : severity_level;
  signal erjtngnpdy : integer;
  signal chvhkh : std_logic_vector(0 to 2);
begin
  atwovqd : entity work.tmyhs
    port map (zbrq => chvhkh, onrmrf => erjtngnpdy, cjris => n, mctoioikzb => rueqtyvfc);
  nyndqi : entity work.tmyhs
    port map (zbrq => chvhkh, onrmrf => erjtngnpdy, cjris => stgsc, mctoioikzb => rueqtyvfc);
  dmcwf : entity work.tmyhs
    port map (zbrq => xsuwxtvsxc, onrmrf => erjtngnpdy, cjris => zaybxb, mctoioikzb => ebhbxlki);
  vjperil : entity work.tmyhs
    port map (zbrq => ortrkxi, onrmrf => gknr, cjris => vqqbjnni, mctoioikzb => rueqtyvfc);
  
  -- Single-driven assignments
  bhmuhmw <= 3 min;
  gknr <= 41322;
  erjtngnpdy <= 8#101#;
  
  -- Multi-driven assignments
  xsuwxtvsxc <= "010";
  ebhbxlki <= 'L';
  xsuwxtvsxc <= "WUH";
end keklswzs;

library ieee;
use ieee.std_logic_1164.all;

entity ai is
  port (zv : buffer integer; s : buffer severity_level; syw : linkage std_logic_vector(0 to 2); rogqvtazrw : in std_logic_vector(4 downto 2));
end ai;

library ieee;
use ieee.std_logic_1164.all;

architecture ekhveut of ai is
  signal ganuqdfpda : time;
  signal gduru : std_logic_vector(0 to 3);
  signal jjqpowqgj : std_logic;
begin
  lm : entity work.ndeahwzj
    port map (rueqtyvfc => jjqpowqgj, vuetanbj => gduru, bhmuhmw => ganuqdfpda);
  
  -- Single-driven assignments
  s <= NOTE;
  zv <= 4_4_2_1;
  
  -- Multi-driven assignments
  jjqpowqgj <= 'H';
end ekhveut;

entity rcmuqdat is
  port (s : out time; amq : inout character);
end rcmuqdat;

library ieee;
use ieee.std_logic_1164.all;

architecture jz of rcmuqdat is
  signal hsj : std_logic_vector(0 to 3);
  signal wgkrj : std_logic;
begin
  stsaeft : entity work.ndeahwzj
    port map (rueqtyvfc => wgkrj, vuetanbj => hsj, bhmuhmw => s);
  
  -- Single-driven assignments
  amq <= 'p';
  
  -- Multi-driven assignments
  hsj <= "XU-W";
end jz;



-- Seed after: 12847367143908614648,3687118713772291287
