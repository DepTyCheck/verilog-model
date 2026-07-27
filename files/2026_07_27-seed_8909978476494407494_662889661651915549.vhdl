-- Seed: 8909978476494407494,662889661651915549

entity us is
  port (mfzfopkc : inout real; yzw : buffer boolean; rnqhqf : inout real; jihzmnsh : out boolean);
end us;

architecture kyhk of us is
  
begin
  -- Single-driven assignments
  mfzfopkc <= 3_1_4_3.3_0_4_2;
  yzw <= jihzmnsh;
end kyhk;

entity rzckukf is
  port (cjirrgw : linkage time);
end rzckukf;

architecture knzzzzih of rzckukf is
  signal jlqmfx : boolean;
  signal vg : real;
  signal bo : boolean;
  signal cz : real;
  signal mhny : boolean;
  signal eqbrsfah : real;
  signal vwghxu : boolean;
  signal tiwcrzvzi : real;
  signal h : boolean;
  signal gr : real;
  signal bdrmmwdqd : boolean;
  signal voiwdvfm : real;
  signal fdutfcm : boolean;
  signal ua : real;
  signal evh : boolean;
  signal gl : real;
begin
  acqwfwqnz : entity work.us
    port map (mfzfopkc => gl, yzw => evh, rnqhqf => ua, jihzmnsh => fdutfcm);
  dqcpen : entity work.us
    port map (mfzfopkc => voiwdvfm, yzw => bdrmmwdqd, rnqhqf => gr, jihzmnsh => h);
  iguijbke : entity work.us
    port map (mfzfopkc => tiwcrzvzi, yzw => vwghxu, rnqhqf => eqbrsfah, jihzmnsh => mhny);
  zmyvi : entity work.us
    port map (mfzfopkc => cz, yzw => bo, rnqhqf => vg, jihzmnsh => jlqmfx);
end knzzzzih;

entity neeybm is
  port (olz : inout time);
end neeybm;

architecture zagqzpp of neeybm is
  signal si : boolean;
  signal ectjkko : real;
  signal dpgofp : boolean;
  signal ssdxyqlrer : real;
  signal pikx : time;
  signal npg : boolean;
  signal hwfox : real;
  signal abexhoqq : boolean;
  signal mkzxo : real;
begin
  c : entity work.us
    port map (mfzfopkc => mkzxo, yzw => abexhoqq, rnqhqf => hwfox, jihzmnsh => npg);
  yqug : entity work.rzckukf
    port map (cjirrgw => olz);
  y : entity work.rzckukf
    port map (cjirrgw => pikx);
  kbfrdbxga : entity work.us
    port map (mfzfopkc => ssdxyqlrer, yzw => dpgofp, rnqhqf => ectjkko, jihzmnsh => si);
end zagqzpp;



-- Seed after: 14674146657818603853,662889661651915549
