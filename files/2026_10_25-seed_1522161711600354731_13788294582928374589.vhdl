-- Seed: 1522161711600354731,13788294582928374589

entity wkgrg is
  port (byrhsichxa : inout real; dwffttw : out bit; wzq : inout time; z : linkage time_vector(4 downto 4));
end wkgrg;

architecture hle of wkgrg is
  
begin
  -- Single-driven assignments
  byrhsichxa <= 16#F.C#;
  dwffttw <= '1';
  wzq <= 4_3.4_2 ps;
end hle;

library ieee;
use ieee.std_logic_1164.all;

entity qpaddds is
  port (xmmvrg : out std_logic_vector(4 downto 1); davy : buffer time);
end qpaddds;

architecture otulxzqg of qpaddds is
  signal jqaidjf : time_vector(4 downto 4);
  signal g : bit;
  signal com : real;
  signal iyffwnyrk : time_vector(4 downto 4);
  signal w : time;
  signal vxyjwdm : bit;
  signal ztlzd : real;
  signal dhatuxleh : time_vector(4 downto 4);
  signal vh : time;
  signal us : bit;
  signal qkdk : real;
  signal kn : time_vector(4 downto 4);
  signal swrjttc : time;
  signal gxaxhwu : bit;
  signal p : real;
begin
  d : entity work.wkgrg
    port map (byrhsichxa => p, dwffttw => gxaxhwu, wzq => swrjttc, z => kn);
  dlsxoetwqq : entity work.wkgrg
    port map (byrhsichxa => qkdk, dwffttw => us, wzq => vh, z => dhatuxleh);
  swvijdbfes : entity work.wkgrg
    port map (byrhsichxa => ztlzd, dwffttw => vxyjwdm, wzq => w, z => iyffwnyrk);
  ezrkrw : entity work.wkgrg
    port map (byrhsichxa => com, dwffttw => g, wzq => davy, z => jqaidjf);
  
  -- Multi-driven assignments
  xmmvrg <= xmmvrg;
end otulxzqg;

entity frlcjqymgy is
  port (liupt : linkage string(4 to 2); twwrlqaq : inout boolean; rth : in integer);
end frlcjqymgy;

library ieee;
use ieee.std_logic_1164.all;

architecture uikyutis of frlcjqymgy is
  signal ehynbbf : time_vector(4 downto 4);
  signal thibrxls : time;
  signal h : bit;
  signal qscb : real;
  signal zytycccoix : time_vector(4 downto 4);
  signal fzdcfyw : time;
  signal sokblsqhfp : bit;
  signal fhumtkii : real;
  signal wnhamwu : time_vector(4 downto 4);
  signal izsnhpus : time;
  signal l : bit;
  signal fqwsczkga : real;
  signal spwmznetr : time;
  signal dwxqvit : std_logic_vector(4 downto 1);
begin
  s : entity work.qpaddds
    port map (xmmvrg => dwxqvit, davy => spwmznetr);
  ytnsbnnusc : entity work.wkgrg
    port map (byrhsichxa => fqwsczkga, dwffttw => l, wzq => izsnhpus, z => wnhamwu);
  ibpim : entity work.wkgrg
    port map (byrhsichxa => fhumtkii, dwffttw => sokblsqhfp, wzq => fzdcfyw, z => zytycccoix);
  zp : entity work.wkgrg
    port map (byrhsichxa => qscb, dwffttw => h, wzq => thibrxls, z => ehynbbf);
  
  -- Single-driven assignments
  twwrlqaq <= twwrlqaq;
  
  -- Multi-driven assignments
  dwxqvit <= dwxqvit;
end uikyutis;



-- Seed after: 2321677800159223786,13788294582928374589
