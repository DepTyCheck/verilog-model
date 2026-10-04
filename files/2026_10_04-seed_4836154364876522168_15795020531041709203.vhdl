-- Seed: 4836154364876522168,15795020531041709203

entity ruh is
  port (abwcqlbsa : inout boolean_vector(1 to 1));
end ruh;

architecture ncz of ruh is
  
begin
  -- Single-driven assignments
  abwcqlbsa <= abwcqlbsa;
end ncz;

entity ufwa is
  port (nrkyqnorgf : in real; hsqksa : out time);
end ufwa;

architecture u of ufwa is
  signal bu : boolean_vector(1 to 1);
  signal fpxhm : boolean_vector(1 to 1);
begin
  fh : entity work.ruh
    port map (abwcqlbsa => fpxhm);
  nmv : entity work.ruh
    port map (abwcqlbsa => bu);
end u;

entity f is
  port (qvqwvpewcc : buffer real);
end f;

architecture ugupaihg of f is
  signal wdlgvljn : boolean_vector(1 to 1);
  signal hfdjz : boolean_vector(1 to 1);
  signal mdsrmt : boolean_vector(1 to 1);
  signal riaa : boolean_vector(1 to 1);
begin
  setcun : entity work.ruh
    port map (abwcqlbsa => riaa);
  tgl : entity work.ruh
    port map (abwcqlbsa => mdsrmt);
  bqrey : entity work.ruh
    port map (abwcqlbsa => hfdjz);
  dcraoyfte : entity work.ruh
    port map (abwcqlbsa => wdlgvljn);
  
  -- Single-driven assignments
  qvqwvpewcc <= 8#7_6_2_7_5.6_2_6#;
end ugupaihg;

library ieee;
use ieee.std_logic_1164.all;

entity hipnkj is
  port (a : out std_logic_vector(3 to 2); u : buffer real; zpqst : out real_vector(1 to 4));
end hipnkj;

architecture vjynstv of hipnkj is
  signal jxnpwy : real;
  signal sacbjuz : boolean_vector(1 to 1);
  signal da : time;
  signal b : real;
begin
  ibazccakui : entity work.ufwa
    port map (nrkyqnorgf => b, hsqksa => da);
  tjyqdbwpau : entity work.ruh
    port map (abwcqlbsa => sacbjuz);
  cxkrmxeg : entity work.f
    port map (qvqwvpewcc => b);
  cywhncnpy : entity work.f
    port map (qvqwvpewcc => jxnpwy);
  
  -- Single-driven assignments
  zpqst <= zpqst;
  u <= jxnpwy;
  
  -- Multi-driven assignments
  a <= a;
  a <= a;
  a <= "";
  a <= (others => '0');
end vjynstv;



-- Seed after: 17548582078791494573,15795020531041709203
