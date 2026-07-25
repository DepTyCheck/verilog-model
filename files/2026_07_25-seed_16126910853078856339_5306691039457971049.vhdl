-- Seed: 16126910853078856339,5306691039457971049

entity ibwhjtp is
  port (eovbzc : inout real; jby : buffer real; tal : out time);
end ibwhjtp;

architecture w of ibwhjtp is
  
begin
  -- Single-driven assignments
  jby <= eovbzc;
end w;

entity hayha is
  port (rtumfaj : inout time; jeuvjllvb : linkage integer_vector(2 downto 0); ke : inout bit_vector(3 to 4); hpnej : buffer real);
end hayha;

architecture exkc of hayha is
  signal yo : time;
  signal mowdspzo : real;
  signal etlmqz : real;
  signal nzzufux : real;
  signal jqdudid : time;
  signal kxft : real;
  signal wqgivqt : real;
begin
  irbgx : entity work.ibwhjtp
    port map (eovbzc => wqgivqt, jby => kxft, tal => jqdudid);
  vyswi : entity work.ibwhjtp
    port map (eovbzc => nzzufux, jby => etlmqz, tal => rtumfaj);
  c : entity work.ibwhjtp
    port map (eovbzc => mowdspzo, jby => hpnej, tal => yo);
end exkc;

entity hq is
  port (jwwfuvk : out time; zrkyobisqv : buffer integer);
end hq;

architecture vcpdtv of hq is
  signal mgycue : real;
  signal ut : real;
  signal nt : real;
  signal fk : bit_vector(3 to 4);
  signal swysopghbb : integer_vector(2 downto 0);
  signal ng : time;
  signal qkvtrruuuv : time;
  signal y : real;
  signal bxsqjakuz : real;
  signal yj : time;
  signal gvfbatk : real;
  signal pwusnbsz : real;
begin
  zxwougs : entity work.ibwhjtp
    port map (eovbzc => pwusnbsz, jby => gvfbatk, tal => yj);
  xwydaqhm : entity work.ibwhjtp
    port map (eovbzc => bxsqjakuz, jby => y, tal => qkvtrruuuv);
  erbjorapr : entity work.hayha
    port map (rtumfaj => ng, jeuvjllvb => swysopghbb, ke => fk, hpnej => nt);
  wgbiouk : entity work.ibwhjtp
    port map (eovbzc => ut, jby => mgycue, tal => jwwfuvk);
  
  -- Single-driven assignments
  zrkyobisqv <= zrkyobisqv;
end vcpdtv;



-- Seed after: 14144869675118932756,5306691039457971049
