-- Seed: 15924572275917295163,4122021602305298647

entity ws is
  port (h : inout character; kc : out real; wcexg : buffer real);
end ws;

architecture hijzuke of ws is
  
begin
  -- Single-driven assignments
  kc <= 2#110.0_0_1#;
  wcexg <= wcexg;
  h <= h;
end hijzuke;

entity exoooivfjq is
  port (syrabr : in time);
end exoooivfjq;

architecture aeseadjxn of exoooivfjq is
  signal vwqeresiv : real;
  signal q : real;
  signal zhyw : character;
  signal zwrgxcttd : real;
  signal a : real;
  signal fbakjfse : character;
  signal txh : real;
  signal n : real;
  signal pqqmiii : character;
begin
  rpf : entity work.ws
    port map (h => pqqmiii, kc => n, wcexg => txh);
  uctecfpdl : entity work.ws
    port map (h => fbakjfse, kc => a, wcexg => zwrgxcttd);
  t : entity work.ws
    port map (h => zhyw, kc => q, wcexg => vwqeresiv);
end aeseadjxn;

entity ghzglo is
  port (swsma : linkage real_vector(0 downto 1));
end ghzglo;

architecture hlffxugs of ghzglo is
  signal yhudfsvsrd : real;
  signal xnqylnmbdl : real;
  signal ouha : character;
  signal tyllkzfap : real;
  signal zorv : real;
  signal xmwmp : character;
begin
  nipp : entity work.ws
    port map (h => xmwmp, kc => zorv, wcexg => tyllkzfap);
  ddk : entity work.ws
    port map (h => ouha, kc => xnqylnmbdl, wcexg => yhudfsvsrd);
end hlffxugs;

entity fhxi is
  port (wfmvzjsbr : buffer real; rnajb : inout severity_level);
end fhxi;

architecture pnms of fhxi is
  signal j : real_vector(0 downto 1);
  signal n : real;
  signal zle : character;
  signal whlu : real;
  signal cqarlhz : real;
  signal ecvi : character;
  signal mz : real;
  signal ttkp : real;
  signal xdtrypkjmz : character;
begin
  p : entity work.ws
    port map (h => xdtrypkjmz, kc => ttkp, wcexg => mz);
  yvdu : entity work.ws
    port map (h => ecvi, kc => cqarlhz, wcexg => whlu);
  mmlxst : entity work.ws
    port map (h => zle, kc => wfmvzjsbr, wcexg => n);
  fsl : entity work.ghzglo
    port map (swsma => j);
  
  -- Single-driven assignments
  rnajb <= ERROR;
end pnms;



-- Seed after: 18396005208343122616,4122021602305298647
