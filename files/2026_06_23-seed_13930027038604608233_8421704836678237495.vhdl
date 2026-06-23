-- Seed: 13930027038604608233,8421704836678237495

entity rqphizb is
  port (svfvsyp : in time);
end rqphizb;

architecture adqpiy of rqphizb is
  
begin
  
end adqpiy;

entity lvsk is
  port (xmwumr : inout severity_level);
end lvsk;

architecture giblj of lvsk is
  
begin
  -- Single-driven assignments
  xmwumr <= FAILURE;
end giblj;

entity ohspnbrwt is
  port (xd : in boolean_vector(2 to 3); okr : in real);
end ohspnbrwt;

architecture kulbpxsldt of ohspnbrwt is
  signal mvgrnjcdi : severity_level;
  signal srzgmr : time;
  signal hwklbnjxd : severity_level;
begin
  psi : entity work.lvsk
    port map (xmwumr => hwklbnjxd);
  jp : entity work.rqphizb
    port map (svfvsyp => srzgmr);
  s : entity work.lvsk
    port map (xmwumr => mvgrnjcdi);
  
  -- Single-driven assignments
  srzgmr <= 1 sec;
end kulbpxsldt;



-- Seed after: 9670510250885277179,8421704836678237495
