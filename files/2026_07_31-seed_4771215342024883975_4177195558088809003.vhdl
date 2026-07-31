-- Seed: 4771215342024883975,4177195558088809003

entity kjyr is
  port (gdrtv : buffer time);
end kjyr;

architecture ub of kjyr is
  
begin
  -- Single-driven assignments
  gdrtv <= 4 sec;
end ub;



-- Seed after: 5837715547712154974,4177195558088809003
