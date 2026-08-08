-- Seed: 7156705954042220163,8927267689619684183

entity otxlbq is
  port (lebjwcrsub : out time);
end otxlbq;

architecture gmpep of otxlbq is
  
begin
  -- Single-driven assignments
  lebjwcrsub <= 3 sec;
end gmpep;

entity n is
  port (tgwjw : buffer integer_vector(2 to 3); f : in integer; jhbgwbfn : in real; wqy : buffer boolean_vector(0 downto 4));
end n;

architecture ejpvr of n is
  signal cxw : time;
  signal tqbs : time;
begin
  fcr : entity work.otxlbq
    port map (lebjwcrsub => tqbs);
  nibpbk : entity work.otxlbq
    port map (lebjwcrsub => cxw);
  
  -- Single-driven assignments
  wqy <= (others => TRUE);
  tgwjw <= (16#62B#, 16#6#);
end ejpvr;



-- Seed after: 13409620717813271267,8927267689619684183
