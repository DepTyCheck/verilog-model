-- Seed: 318258004790174780,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity sgs is
  port (qnc : in integer; tvti : inout std_logic_vector(1 to 3); asrtvagll : out std_logic);
end sgs;

architecture nquijpqkk of sgs is
  
begin
  -- Multi-driven assignments
  tvti <= "WWH";
  asrtvagll <= '-';
end nquijpqkk;

library ieee;
use ieee.std_logic_1164.all;

entity axtcqent is
  port (op : inout std_logic_vector(3 downto 0); k : linkage std_logic_vector(3 downto 2); hzasxic : inout integer; wfmprhc : linkage boolean);
end axtcqent;

library ieee;
use ieee.std_logic_1164.all;

architecture zxqod of axtcqent is
  signal rwpdnq : std_logic;
  signal jpdikkki : std_logic_vector(1 to 3);
  signal zxeddmr : integer;
  signal dfwnvum : std_logic;
  signal jkw : std_logic_vector(1 to 3);
begin
  fshxagir : entity work.sgs
    port map (qnc => hzasxic, tvti => jkw, asrtvagll => dfwnvum);
  jv : entity work.sgs
    port map (qnc => zxeddmr, tvti => jpdikkki, asrtvagll => rwpdnq);
  
  -- Single-driven assignments
  hzasxic <= 2#0_1_1_0#;
  zxeddmr <= 2#0#;
end zxqod;

entity ulvsczzpi is
  port (nlqbuhn : buffer time_vector(1 to 0); q : out boolean; ykglvcempz : buffer time);
end ulvsczzpi;

library ieee;
use ieee.std_logic_1164.all;

architecture k of ulvsczzpi is
  signal nfydh : boolean;
  signal ilnvyesdva : std_logic_vector(3 downto 2);
  signal idktcrux : std_logic_vector(3 downto 0);
  signal hmiiwt : integer;
  signal zvgxr : std_logic;
  signal px : std_logic_vector(1 to 3);
  signal xpau : integer;
begin
  qsbjve : entity work.sgs
    port map (qnc => xpau, tvti => px, asrtvagll => zvgxr);
  vgixctwcdu : entity work.sgs
    port map (qnc => hmiiwt, tvti => px, asrtvagll => zvgxr);
  opgokvq : entity work.axtcqent
    port map (op => idktcrux, k => ilnvyesdva, hzasxic => xpau, wfmprhc => nfydh);
  
  -- Single-driven assignments
  ykglvcempz <= 1 hr;
  hmiiwt <= 4_2_3_4_2;
  nlqbuhn <= (others => 0 ns);
  q <= TRUE;
end k;

entity gqdfdport is
  port (nfc : out time; hbdfbbt : out time_vector(2 downto 3));
end gqdfdport;

library ieee;
use ieee.std_logic_1164.all;

architecture epl of gqdfdport is
  signal psbay : std_logic_vector(1 to 3);
  signal mcu : std_logic;
  signal yuert : std_logic_vector(1 to 3);
  signal ualaxyekja : integer;
begin
  zfygin : entity work.sgs
    port map (qnc => ualaxyekja, tvti => yuert, asrtvagll => mcu);
  dgmvppjnhs : entity work.sgs
    port map (qnc => ualaxyekja, tvti => psbay, asrtvagll => mcu);
  
  -- Single-driven assignments
  hbdfbbt <= (others => 0 ns);
  
  -- Multi-driven assignments
  mcu <= '1';
  yuert <= "H-H";
  yuert <= "UH0";
  psbay <= ('Z', 'U', 'W');
end epl;



-- Seed after: 327823931260854870,5472058987609252853
