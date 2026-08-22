-- Seed: 13967152925845218900,5805648483995786113

entity fjncr is
  port (nsqlnnfdz : inout integer_vector(3 to 3); lgjaupnfqc : in time);
end fjncr;

architecture rynngfo of fjncr is
  
begin
  -- Single-driven assignments
  nsqlnnfdz <= nsqlnnfdz;
end rynngfo;

entity a is
  port (fla : buffer integer_vector(2 to 3); icsjxio : out bit_vector(2 downto 4); dfct : linkage integer);
end a;

architecture dkdms of a is
  signal p : time;
  signal asmojd : integer_vector(3 to 3);
  signal hzbclpdcp : time;
  signal sgmn : integer_vector(3 to 3);
  signal bzjft : integer_vector(3 to 3);
  signal dv : time;
  signal hzz : integer_vector(3 to 3);
begin
  auw : entity work.fjncr
    port map (nsqlnnfdz => hzz, lgjaupnfqc => dv);
  vtgl : entity work.fjncr
    port map (nsqlnnfdz => bzjft, lgjaupnfqc => dv);
  mywu : entity work.fjncr
    port map (nsqlnnfdz => sgmn, lgjaupnfqc => hzbclpdcp);
  von : entity work.fjncr
    port map (nsqlnnfdz => asmojd, lgjaupnfqc => p);
end dkdms;

entity jl is
  port (tux : buffer bit);
end jl;

architecture z of jl is
  signal gujzcwxf : time;
  signal vqpp : integer_vector(3 to 3);
  signal rfggebaab : integer_vector(3 to 3);
  signal ljibanhrwk : time;
  signal xtebbwh : integer_vector(3 to 3);
begin
  ddnajsqv : entity work.fjncr
    port map (nsqlnnfdz => xtebbwh, lgjaupnfqc => ljibanhrwk);
  mapqnmdypd : entity work.fjncr
    port map (nsqlnnfdz => rfggebaab, lgjaupnfqc => ljibanhrwk);
  ixzlovv : entity work.fjncr
    port map (nsqlnnfdz => vqpp, lgjaupnfqc => gujzcwxf);
  
  -- Single-driven assignments
  ljibanhrwk <= ljibanhrwk;
  tux <= tux;
  gujzcwxf <= ljibanhrwk;
end z;



-- Seed after: 6673957962557887880,5805648483995786113
