-- Seed: 12445785872466728297,7198033922882419595

entity nqlppdicxx is
  port (zn : buffer real);
end nqlppdicxx;

architecture apudoeb of nqlppdicxx is
  
begin
  -- Single-driven assignments
  zn <= 16#CB1.E9A#;
end apudoeb;

entity v is
  port (tdcgwi : linkage severity_level; spcfcpv : out time);
end v;

architecture mtyjp of v is
  signal sormb : real;
  signal gldkice : real;
begin
  iaui : entity work.nqlppdicxx
    port map (zn => gldkice);
  umfnonq : entity work.nqlppdicxx
    port map (zn => sormb);
  
  -- Single-driven assignments
  spcfcpv <= 8#253# ms;
end mtyjp;



-- Seed after: 8262771510729699037,7198033922882419595
