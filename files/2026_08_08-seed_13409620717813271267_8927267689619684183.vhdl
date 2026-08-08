-- Seed: 13409620717813271267,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity hde is
  port (eahuze : buffer real; wki : buffer severity_level; gun : out std_logic_vector(1 downto 3));
end hde;

architecture hsnnb of hde is
  
begin
  -- Multi-driven assignments
  gun <= (others => '0');
  gun <= "";
  gun <= gun;
  gun <= gun;
end hsnnb;

library ieee;
use ieee.std_logic_1164.all;

entity kgkyxw is
  port (n : linkage std_logic; ggdpqy : inout integer; iob : out real; ylzm : buffer integer);
end kgkyxw;

library ieee;
use ieee.std_logic_1164.all;

architecture zzokabho of kgkyxw is
  signal llgkdreej : std_logic_vector(1 downto 3);
  signal ncw : severity_level;
begin
  bohn : entity work.hde
    port map (eahuze => iob, wki => ncw, gun => llgkdreej);
  
  -- Single-driven assignments
  ylzm <= 2#101#;
  ggdpqy <= ylzm;
  
  -- Multi-driven assignments
  llgkdreej <= llgkdreej;
  llgkdreej <= (others => '0');
  llgkdreej <= llgkdreej;
end zzokabho;

library ieee;
use ieee.std_logic_1164.all;

entity pkkndbya is
  port (lpf : linkage real; gwitmruzyy : out real; nlsd : out real; xzcrhqdx : out std_logic_vector(0 downto 4));
end pkkndbya;

library ieee;
use ieee.std_logic_1164.all;

architecture uhdxjveozc of pkkndbya is
  signal f : severity_level;
  signal slhhd : severity_level;
  signal yhtxtc : std_logic_vector(1 downto 3);
  signal udxvwm : severity_level;
  signal eavfytk : real;
  signal lvmjousvt : severity_level;
  signal rdpqsmn : real;
begin
  qtjjvkig : entity work.hde
    port map (eahuze => rdpqsmn, wki => lvmjousvt, gun => xzcrhqdx);
  vellvik : entity work.hde
    port map (eahuze => eavfytk, wki => udxvwm, gun => yhtxtc);
  susuehf : entity work.hde
    port map (eahuze => gwitmruzyy, wki => slhhd, gun => xzcrhqdx);
  bidgj : entity work.hde
    port map (eahuze => nlsd, wki => f, gun => xzcrhqdx);
  
  -- Multi-driven assignments
  yhtxtc <= (others => '0');
end uhdxjveozc;

entity imazhvesfa is
  port (dufzzex : inout time; m : linkage integer);
end imazhvesfa;

library ieee;
use ieee.std_logic_1164.all;

architecture gp of imazhvesfa is
  signal hgvgpqe : std_logic_vector(0 downto 4);
  signal cbhkywsl : real;
  signal o : real;
  signal zbfzyb : real;
  signal xkxtcmw : std_logic_vector(1 downto 3);
  signal mhxzh : severity_level;
  signal zsmycivxhb : real;
  signal qshhczmbnh : integer;
  signal efjiq : real;
  signal ylpinx : integer;
  signal dsuomrhp : std_logic;
begin
  ldbto : entity work.kgkyxw
    port map (n => dsuomrhp, ggdpqy => ylpinx, iob => efjiq, ylzm => qshhczmbnh);
  rujd : entity work.hde
    port map (eahuze => zsmycivxhb, wki => mhxzh, gun => xkxtcmw);
  iqgkuyvr : entity work.pkkndbya
    port map (lpf => zbfzyb, gwitmruzyy => o, nlsd => cbhkywsl, xzcrhqdx => hgvgpqe);
  
  -- Single-driven assignments
  dufzzex <= 8#1_0# fs;
  
  -- Multi-driven assignments
  dsuomrhp <= 'X';
end gp;



-- Seed after: 273523842504157706,8927267689619684183
