-- Seed: 637183367174812231,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity wsog is
  port (edrdi : in std_logic_vector(4 to 4));
end wsog;

architecture bode of wsog is
  
begin
  
end bode;

entity es is
  port (oowrez : in bit; jukdrwjffq : linkage real; jsmqqa : in real);
end es;

library ieee;
use ieee.std_logic_1164.all;

architecture nusfyafw of es is
  signal pritt : std_logic_vector(4 to 4);
  signal cwcdtltfr : std_logic_vector(4 to 4);
begin
  pqbbkzc : entity work.wsog
    port map (edrdi => cwcdtltfr);
  soeemgqc : entity work.wsog
    port map (edrdi => pritt);
  
  -- Multi-driven assignments
  cwcdtltfr <= "Z";
  cwcdtltfr <= (others => 'L');
  cwcdtltfr <= cwcdtltfr;
  cwcdtltfr <= cwcdtltfr;
end nusfyafw;

library ieee;
use ieee.std_logic_1164.all;

entity myrzoot is
  port (mfmxkiffj : out std_logic_vector(0 downto 1); xp : linkage std_logic_vector(1 downto 3));
end myrzoot;

library ieee;
use ieee.std_logic_1164.all;

architecture k of myrzoot is
  signal lktbrusvvb : std_logic_vector(4 to 4);
begin
  yoa : entity work.wsog
    port map (edrdi => lktbrusvvb);
  
  -- Multi-driven assignments
  mfmxkiffj <= (others => '0');
  mfmxkiffj <= mfmxkiffj;
  mfmxkiffj <= (others => '0');
end k;

entity mtjpbnksrb is
  port (hearlnj : buffer real; qshmes : inout character; pgxhra : inout time; hcqsqh : linkage severity_level);
end mtjpbnksrb;

library ieee;
use ieee.std_logic_1164.all;

architecture tornmxusp of mtjpbnksrb is
  signal fl : std_logic_vector(4 to 4);
  signal uvaffm : real;
  signal xpbqftgc : bit;
  signal vqmqzkqrj : std_logic_vector(1 downto 3);
  signal demhunu : std_logic_vector(0 downto 1);
begin
  gycnx : entity work.myrzoot
    port map (mfmxkiffj => demhunu, xp => vqmqzkqrj);
  cfqldcyth : entity work.es
    port map (oowrez => xpbqftgc, jukdrwjffq => hearlnj, jsmqqa => uvaffm);
  jthftr : entity work.wsog
    port map (edrdi => fl);
  lkxybe : entity work.wsog
    port map (edrdi => fl);
  
  -- Single-driven assignments
  qshmes <= qshmes;
  xpbqftgc <= xpbqftgc;
  pgxhra <= pgxhra;
  
  -- Multi-driven assignments
  fl <= fl;
  vqmqzkqrj <= demhunu;
  demhunu <= "";
end tornmxusp;



-- Seed after: 18081868672244571974,12143220691580258643
