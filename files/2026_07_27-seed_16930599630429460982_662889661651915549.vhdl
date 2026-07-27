-- Seed: 16930599630429460982,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity zplavd is
  port (mbpisr : in integer; jf : linkage severity_level; lisbtxpln : buffer std_logic_vector(0 downto 3));
end zplavd;

architecture rgstgj of zplavd is
  
begin
  
end rgstgj;

entity ke is
  port (aqsied : out character; cfgi : linkage time);
end ke;

library ieee;
use ieee.std_logic_1164.all;

architecture af of ke is
  signal jvhbmev : severity_level;
  signal ldgcphxl : severity_level;
  signal qurhvbh : integer;
  signal hpuehnuxam : std_logic_vector(0 downto 3);
  signal dogdis : severity_level;
  signal essfzbvcj : integer;
  signal lnwjlbsad : std_logic_vector(0 downto 3);
  signal urzo : severity_level;
  signal lolxhc : integer;
begin
  avjjrcter : entity work.zplavd
    port map (mbpisr => lolxhc, jf => urzo, lisbtxpln => lnwjlbsad);
  frqzlgx : entity work.zplavd
    port map (mbpisr => essfzbvcj, jf => dogdis, lisbtxpln => hpuehnuxam);
  iiux : entity work.zplavd
    port map (mbpisr => qurhvbh, jf => ldgcphxl, lisbtxpln => hpuehnuxam);
  kpb : entity work.zplavd
    port map (mbpisr => lolxhc, jf => jvhbmev, lisbtxpln => lnwjlbsad);
  
  -- Single-driven assignments
  aqsied <= 'l';
end af;



-- Seed after: 8909978476494407494,662889661651915549
