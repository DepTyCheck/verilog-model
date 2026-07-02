-- Seed: 949614335485002583,13694093582652240945

entity nndd is
  port (vtmlgmkk : out time; lmifdavdtn : linkage real);
end nndd;

architecture skzyszt of nndd is
  
begin
  -- Single-driven assignments
  vtmlgmkk <= 1 hr;
end skzyszt;

entity mkrzacumzr is
  port (leyhsnoj : buffer integer; wshvx : in bit; pxlqrvss : linkage boolean_vector(2 to 2));
end mkrzacumzr;

architecture dle of mkrzacumzr is
  signal ueoqf : real;
  signal nngfbksi : time;
  signal ezchnqks : real;
  signal ffsvikyur : time;
begin
  xsauikqwr : entity work.nndd
    port map (vtmlgmkk => ffsvikyur, lmifdavdtn => ezchnqks);
  nay : entity work.nndd
    port map (vtmlgmkk => nngfbksi, lmifdavdtn => ueoqf);
  
  -- Single-driven assignments
  leyhsnoj <= 2#1_0_1_1_0#;
end dle;

library ieee;
use ieee.std_logic_1164.all;

entity sybxgccewu is
  port (qrvoai : buffer std_logic);
end sybxgccewu;

architecture eusl of sybxgccewu is
  signal oysrsxf : real;
  signal dmplvp : time;
  signal iqkzuztd : boolean_vector(2 to 2);
  signal yr : integer;
  signal xnrvdc : boolean_vector(2 to 2);
  signal i : bit;
  signal lviqtpd : integer;
  signal fxdbfyqmc : real;
  signal yje : time;
begin
  quftmwdgjy : entity work.nndd
    port map (vtmlgmkk => yje, lmifdavdtn => fxdbfyqmc);
  vqpyhbkatd : entity work.mkrzacumzr
    port map (leyhsnoj => lviqtpd, wshvx => i, pxlqrvss => xnrvdc);
  zeo : entity work.mkrzacumzr
    port map (leyhsnoj => yr, wshvx => i, pxlqrvss => iqkzuztd);
  oaqolzql : entity work.nndd
    port map (vtmlgmkk => dmplvp, lmifdavdtn => oysrsxf);
  
  -- Multi-driven assignments
  qrvoai <= 'H';
  qrvoai <= 'U';
  qrvoai <= '-';
  qrvoai <= '-';
end eusl;

entity eunagsqjr is
  port (hvaehurg : out bit; hxdrwmhid : linkage boolean_vector(0 to 1); mkpivz : buffer real);
end eunagsqjr;

library ieee;
use ieee.std_logic_1164.all;

architecture gcs of eunagsqjr is
  signal zhdlbs : std_logic;
begin
  uwqsi : entity work.sybxgccewu
    port map (qrvoai => zhdlbs);
  rf : entity work.sybxgccewu
    port map (qrvoai => zhdlbs);
  jk : entity work.sybxgccewu
    port map (qrvoai => zhdlbs);
  
  -- Single-driven assignments
  mkpivz <= 0_3_4_2_1.2_3_2_4;
end gcs;



-- Seed after: 15644621668354832696,13694093582652240945
