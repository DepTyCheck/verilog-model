-- Seed: 10124937657826689653,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity tnb is
  port (obevqo : linkage real; evqxolob : buffer std_logic; gtnuofh : out time; od : buffer time);
end tnb;

architecture efuuwq of tnb is
  
begin
  -- Multi-driven assignments
  evqxolob <= 'H';
end efuuwq;

entity lybavp is
  port (cfqg : out real; p : out time);
end lybavp;

library ieee;
use ieee.std_logic_1164.all;

architecture jtpufilfu of lybavp is
  signal qqkvzymgue : time;
  signal ucwgh : time;
  signal uhv : time;
  signal haaa : real;
  signal w : time;
  signal ht : time;
  signal whzfcd : std_logic;
  signal yuotxqu : real;
  signal ayq : time;
  signal cvf : time;
  signal tzmcdlwug : std_logic;
  signal els : real;
begin
  gxjuck : entity work.tnb
    port map (obevqo => els, evqxolob => tzmcdlwug, gtnuofh => cvf, od => ayq);
  bvcy : entity work.tnb
    port map (obevqo => yuotxqu, evqxolob => whzfcd, gtnuofh => ht, od => w);
  mlgoqj : entity work.tnb
    port map (obevqo => haaa, evqxolob => tzmcdlwug, gtnuofh => uhv, od => ucwgh);
  btuir : entity work.tnb
    port map (obevqo => cfqg, evqxolob => tzmcdlwug, gtnuofh => p, od => qqkvzymgue);
end jtpufilfu;

entity kemtqfqkej is
  port (d : buffer integer);
end kemtqfqkej;

library ieee;
use ieee.std_logic_1164.all;

architecture wp of kemtqfqkej is
  signal cyxvvb : time;
  signal ze : time;
  signal xgko : std_logic;
  signal lcdnxcmser : real;
  signal cokuglg : time;
  signal rqee : real;
begin
  vnaiuenxdl : entity work.lybavp
    port map (cfqg => rqee, p => cokuglg);
  wjh : entity work.tnb
    port map (obevqo => lcdnxcmser, evqxolob => xgko, gtnuofh => ze, od => cyxvvb);
  
  -- Multi-driven assignments
  xgko <= 'Z';
  xgko <= '0';
end wp;

entity oh is
  port (lt : out real; cv : in real; woum : out integer);
end oh;

architecture ethjybrd of oh is
  
begin
  -- Single-driven assignments
  lt <= 02.02;
  woum <= 16#B_8_6_B#;
end ethjybrd;



-- Seed after: 9893231186816900078,4860866131898729603
