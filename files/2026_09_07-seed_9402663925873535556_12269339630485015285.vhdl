-- Seed: 9402663925873535556,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity hkdduhvwka is
  port ( hv : buffer std_logic_vector(4 downto 4)
  ; pb : out real_vector(0 downto 3)
  ; dltgxrdx : out std_logic_vector(0 to 3)
  ; wbwprxvj : inout std_logic
  );
end hkdduhvwka;

architecture ulofk of hkdduhvwka is
  
begin
  -- Multi-driven assignments
  wbwprxvj <= wbwprxvj;
end ulofk;

entity yk is
  port (xcrkravgo : in severity_level; akfv : out real; qbudqkskl : in real; w : in integer_vector(4 to 1));
end yk;

library ieee;
use ieee.std_logic_1164.all;

architecture df of yk is
  signal n : std_logic_vector(0 to 3);
  signal pna : real_vector(0 downto 3);
  signal lpapcoyls : std_logic_vector(4 downto 4);
  signal iffsdm : std_logic;
  signal qqp : std_logic_vector(0 to 3);
  signal lnyfonuu : real_vector(0 downto 3);
  signal vmihvz : std_logic_vector(4 downto 4);
  signal kgjiaqji : real_vector(0 downto 3);
  signal xsmlqkw : std_logic;
  signal mn : std_logic_vector(0 to 3);
  signal wfjemdlom : real_vector(0 downto 3);
  signal feiwbcdhvz : std_logic_vector(4 downto 4);
begin
  meirev : entity work.hkdduhvwka
    port map (hv => feiwbcdhvz, pb => wfjemdlom, dltgxrdx => mn, wbwprxvj => xsmlqkw);
  mkgbti : entity work.hkdduhvwka
    port map (hv => feiwbcdhvz, pb => kgjiaqji, dltgxrdx => mn, wbwprxvj => xsmlqkw);
  uerkns : entity work.hkdduhvwka
    port map (hv => vmihvz, pb => lnyfonuu, dltgxrdx => qqp, wbwprxvj => iffsdm);
  fam : entity work.hkdduhvwka
    port map (hv => lpapcoyls, pb => pna, dltgxrdx => n, wbwprxvj => xsmlqkw);
  
  -- Single-driven assignments
  akfv <= qbudqkskl;
  
  -- Multi-driven assignments
  vmihvz <= "-";
  iffsdm <= 'H';
end df;

entity cavfpiu is
  port (pcxprfyt : in real);
end cavfpiu;

library ieee;
use ieee.std_logic_1164.all;

architecture z of cavfpiu is
  signal mgzyxe : integer_vector(4 to 1);
  signal zlqxo : real;
  signal sfyyscwyhb : severity_level;
  signal ycqkszcm : std_logic_vector(0 to 3);
  signal wf : real_vector(0 downto 3);
  signal mykam : std_logic_vector(4 downto 4);
  signal o : real_vector(0 downto 3);
  signal wkk : std_logic;
  signal ejopl : std_logic_vector(0 to 3);
  signal mfcz : real_vector(0 downto 3);
  signal qtb : std_logic_vector(4 downto 4);
begin
  pqoi : entity work.hkdduhvwka
    port map (hv => qtb, pb => mfcz, dltgxrdx => ejopl, wbwprxvj => wkk);
  nhe : entity work.hkdduhvwka
    port map (hv => qtb, pb => o, dltgxrdx => ejopl, wbwprxvj => wkk);
  ri : entity work.hkdduhvwka
    port map (hv => mykam, pb => wf, dltgxrdx => ycqkszcm, wbwprxvj => wkk);
  mtd : entity work.yk
    port map (xcrkravgo => sfyyscwyhb, akfv => zlqxo, qbudqkskl => pcxprfyt, w => mgzyxe);
  
  -- Single-driven assignments
  sfyyscwyhb <= ERROR;
  mgzyxe <= (others => 0);
end z;



-- Seed after: 13456898506173777166,12269339630485015285
