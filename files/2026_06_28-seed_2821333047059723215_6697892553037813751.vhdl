-- Seed: 2821333047059723215,6697892553037813751

entity gfa is
  port (vdycdweu : out real; tn : buffer real; bbcxn : out bit_vector(0 downto 4));
end gfa;

architecture rckbxrvvj of gfa is
  
begin
  -- Single-driven assignments
  tn <= 2#1_1_0.1_0#;
  bbcxn <= (others => '0');
  vdycdweu <= 16#A834.0#;
end rckbxrvvj;

library ieee;
use ieee.std_logic_1164.all;

entity pzzkuroppw is
  port (yjsco : inout std_logic_vector(4 downto 2); sg : out time; tnzls : in severity_level; auvhda : inout real);
end pzzkuroppw;

architecture fur of pzzkuroppw is
  
begin
  -- Single-driven assignments
  auvhda <= 2#1_1_1_1.0#;
end fur;

library ieee;
use ieee.std_logic_1164.all;

entity dscp is
  port (ljcxar : inout std_logic; yovdeghvz : inout integer);
end dscp;

library ieee;
use ieee.std_logic_1164.all;

architecture cac of dscp is
  signal anrrarcq : real;
  signal w : severity_level;
  signal qhezjr : time;
  signal z : std_logic_vector(4 downto 2);
  signal stji : bit_vector(0 downto 4);
  signal ght : real;
  signal y : real;
  signal yobghrk : bit_vector(0 downto 4);
  signal jowva : real;
  signal aaftrfx : real;
begin
  wev : entity work.gfa
    port map (vdycdweu => aaftrfx, tn => jowva, bbcxn => yobghrk);
  ettgds : entity work.gfa
    port map (vdycdweu => y, tn => ght, bbcxn => stji);
  axatvyfdq : entity work.pzzkuroppw
    port map (yjsco => z, sg => qhezjr, tnzls => w, auvhda => anrrarcq);
  
  -- Single-driven assignments
  yovdeghvz <= 8#0_7#;
  w <= WARNING;
  
  -- Multi-driven assignments
  ljcxar <= 'U';
end cac;

entity mog is
  port (h : out time);
end mog;

library ieee;
use ieee.std_logic_1164.all;

architecture an of mog is
  signal hcx : bit_vector(0 downto 4);
  signal yya : real;
  signal ncb : real;
  signal xov : bit_vector(0 downto 4);
  signal wu : real;
  signal dxm : real;
  signal e : integer;
  signal lgpizhmakh : std_logic;
  signal diswdgwd : bit_vector(0 downto 4);
  signal rfeaykp : real;
  signal io : real;
begin
  xwaddjje : entity work.gfa
    port map (vdycdweu => io, tn => rfeaykp, bbcxn => diswdgwd);
  uym : entity work.dscp
    port map (ljcxar => lgpizhmakh, yovdeghvz => e);
  usieqerthc : entity work.gfa
    port map (vdycdweu => dxm, tn => wu, bbcxn => xov);
  i : entity work.gfa
    port map (vdycdweu => ncb, tn => yya, bbcxn => hcx);
  
  -- Single-driven assignments
  h <= 16#9_8_0.7_0_3_5_5# ns;
  
  -- Multi-driven assignments
  lgpizhmakh <= '-';
  lgpizhmakh <= '1';
  lgpizhmakh <= 'X';
  lgpizhmakh <= 'W';
end an;



-- Seed after: 6124739282267902026,6697892553037813751
