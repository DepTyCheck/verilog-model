-- Seed: 16857719899205059294,3426943171337919827

entity yon is
  port (fy : buffer boolean_vector(0 to 2));
end yon;

architecture dpi of yon is
  
begin
  -- Single-driven assignments
  fy <= (FALSE, TRUE, TRUE);
end dpi;

entity wqzgdvie is
  port (thwyspttdq : in integer; lzdyfivjm : buffer real);
end wqzgdvie;

architecture hfpnmmuw of wqzgdvie is
  signal sv : boolean_vector(0 to 2);
begin
  yysxmyaz : entity work.yon
    port map (fy => sv);
  
  -- Single-driven assignments
  lzdyfivjm <= 2.00003;
end hfpnmmuw;



-- Seed after: 7301937089356986264,3426943171337919827
