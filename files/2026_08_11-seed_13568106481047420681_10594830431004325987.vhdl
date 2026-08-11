-- Seed: 13568106481047420681,10594830431004325987

entity vvxvo is
  port (cmbbgasl : buffer integer);
end vvxvo;

architecture wfyfayouo of vvxvo is
  
begin
  -- Single-driven assignments
  cmbbgasl <= 2;
end wfyfayouo;

entity bdpfzfdo is
  port (psq : out real; jpxkztab : buffer real);
end bdpfzfdo;

architecture ieogqt of bdpfzfdo is
  signal plpbxk : integer;
  signal ylihgrqpu : integer;
  signal rbfqqdj : integer;
  signal ngqdb : integer;
begin
  vhslp : entity work.vvxvo
    port map (cmbbgasl => ngqdb);
  umrwesd : entity work.vvxvo
    port map (cmbbgasl => rbfqqdj);
  kcbyhb : entity work.vvxvo
    port map (cmbbgasl => ylihgrqpu);
  taicyblnvi : entity work.vvxvo
    port map (cmbbgasl => plpbxk);
  
  -- Single-driven assignments
  jpxkztab <= 8#6171.7_0#;
  psq <= 3.1_4_0_0_0;
end ieogqt;

entity uvar is
  port (i : inout severity_level);
end uvar;

architecture vd of uvar is
  signal zjlnuq : integer;
  signal zwfrisbl : real;
  signal dnhspnz : real;
  signal pzugoss : integer;
  signal zvkocrwaac : integer;
begin
  ggnwea : entity work.vvxvo
    port map (cmbbgasl => zvkocrwaac);
  vazfdumkkg : entity work.vvxvo
    port map (cmbbgasl => pzugoss);
  xwngsp : entity work.bdpfzfdo
    port map (psq => dnhspnz, jpxkztab => zwfrisbl);
  qkwq : entity work.vvxvo
    port map (cmbbgasl => zjlnuq);
end vd;

library ieee;
use ieee.std_logic_1164.all;

entity ocgjshkwp is
  port (dqtp : linkage std_logic; antxqh : inout time; daawxutjn : buffer severity_level);
end ocgjshkwp;

architecture ne of ocgjshkwp is
  signal mdqf : integer;
  signal s : integer;
  signal n : real;
  signal tvtq : real;
  signal xnsljx : real;
  signal cadu : real;
begin
  se : entity work.bdpfzfdo
    port map (psq => cadu, jpxkztab => xnsljx);
  mvcv : entity work.bdpfzfdo
    port map (psq => tvtq, jpxkztab => n);
  migd : entity work.vvxvo
    port map (cmbbgasl => s);
  nyxsb : entity work.vvxvo
    port map (cmbbgasl => mdqf);
  
  -- Single-driven assignments
  daawxutjn <= ERROR;
  antxqh <= 16#E_7_7_0_3# us;
end ne;



-- Seed after: 4955261095067171388,10594830431004325987
