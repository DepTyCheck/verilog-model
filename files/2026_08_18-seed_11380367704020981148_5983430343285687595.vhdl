-- Seed: 11380367704020981148,5983430343285687595

entity emusyj is
  port (tsbmwrkpz : in boolean_vector(3 downto 4));
end emusyj;

architecture geyogq of emusyj is
  
begin
  
end geyogq;

entity bxavvfl is
  port (zdnmtmop : linkage real_vector(3 downto 3); bzirh : out severity_level);
end bxavvfl;

architecture zeluqi of bxavvfl is
  signal nzlt : boolean_vector(3 downto 4);
  signal tlq : boolean_vector(3 downto 4);
  signal nxgrqpe : boolean_vector(3 downto 4);
begin
  komqgy : entity work.emusyj
    port map (tsbmwrkpz => nxgrqpe);
  qwgw : entity work.emusyj
    port map (tsbmwrkpz => tlq);
  dkm : entity work.emusyj
    port map (tsbmwrkpz => nxgrqpe);
  jegmjibso : entity work.emusyj
    port map (tsbmwrkpz => nzlt);
  
  -- Single-driven assignments
  nxgrqpe <= nxgrqpe;
  tlq <= nxgrqpe;
  bzirh <= WARNING;
  nzlt <= (others => TRUE);
end zeluqi;



-- Seed after: 9468261797605859754,5983430343285687595
