-- Seed: 8560944026441053639,1834764876137802293

entity bqu is
  port (chlrvjmag : inout time; yly : linkage time; bmmglko : buffer integer);
end bqu;

architecture wz of bqu is
  
begin
  -- Single-driven assignments
  chlrvjmag <= 3 min;
end wz;

entity ilms is
  port (adqmtsqmjx : linkage bit);
end ilms;

architecture f of ilms is
  signal drhpzgwu : integer;
  signal emtwmtpmx : time;
  signal ctxero : time;
  signal lrustjlau : integer;
  signal b : time;
  signal iqdvj : time;
  signal xzygs : integer;
  signal m : time;
  signal omfm : time;
begin
  blwdwkjpr : entity work.bqu
    port map (chlrvjmag => omfm, yly => m, bmmglko => xzygs);
  kokyxwytj : entity work.bqu
    port map (chlrvjmag => iqdvj, yly => b, bmmglko => lrustjlau);
  cvfh : entity work.bqu
    port map (chlrvjmag => ctxero, yly => emtwmtpmx, bmmglko => drhpzgwu);
end f;

entity lnnt is
  port (psjtkp : out integer_vector(1 to 2); dtgsrcvqu : linkage integer; wwb : in time; dc : buffer time);
end lnnt;

architecture vtswkx of lnnt is
  signal uiv : bit;
  signal erswfhuhkc : integer;
  signal f : time;
  signal elm : integer;
  signal fciaruuw : time;
  signal zkuj : time;
begin
  x : entity work.bqu
    port map (chlrvjmag => zkuj, yly => fciaruuw, bmmglko => elm);
  wy : entity work.bqu
    port map (chlrvjmag => dc, yly => f, bmmglko => erswfhuhkc);
  bucpeexsiy : entity work.ilms
    port map (adqmtsqmjx => uiv);
  
  -- Single-driven assignments
  psjtkp <= (34432, 4243);
end vtswkx;



-- Seed after: 16404494758660484376,1834764876137802293
