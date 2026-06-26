-- Seed: 14470370007225536583,12011142928354116943

entity irketp is
  port (qlpjanc : in real; hl : out integer);
end irketp;

architecture uhjq of irketp is
  
begin
  -- Single-driven assignments
  hl <= 0_1_2_4;
end uhjq;

entity fszptt is
  port (owv : out time; dfclokix : buffer integer);
end fszptt;

architecture kqxd of fszptt is
  
begin
  -- Single-driven assignments
  owv <= 2_4_3.3_2 ps;
  dfclokix <= 8#5#;
end kqxd;

entity aiacatnzxs is
  port (tr : buffer bit; xev : buffer integer; fxtctkofz : inout severity_level);
end aiacatnzxs;

architecture n of aiacatnzxs is
  signal ofhrnoejv : integer;
  signal ijm : real;
  signal libvqhe : real;
  signal fqyokk : integer;
  signal a : time;
  signal vnaflhaw : integer;
  signal wsfv : real;
begin
  zjwttmsqdb : entity work.irketp
    port map (qlpjanc => wsfv, hl => vnaflhaw);
  ywb : entity work.fszptt
    port map (owv => a, dfclokix => fqyokk);
  jnlryvdqk : entity work.irketp
    port map (qlpjanc => libvqhe, hl => xev);
  j : entity work.irketp
    port map (qlpjanc => ijm, hl => ofhrnoejv);
end n;



-- Seed after: 290647271798002811,12011142928354116943
