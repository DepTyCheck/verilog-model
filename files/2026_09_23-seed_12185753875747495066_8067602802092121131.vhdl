-- Seed: 12185753875747495066,8067602802092121131

entity ibp is
  port (nf : buffer time);
end ibp;

architecture a of ibp is
  
begin
  -- Single-driven assignments
  nf <= 1_0_4_1_0.3201 ns;
end a;



-- Seed after: 2204476408952598863,8067602802092121131
