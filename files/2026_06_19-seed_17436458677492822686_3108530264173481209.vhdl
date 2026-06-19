-- Seed: 17436458677492822686,3108530264173481209

entity azf is
  port (dnwtep : inout time; l : buffer integer);
end azf;

architecture ck of azf is
  
begin
  -- Single-driven assignments
  dnwtep <= 16#5_1_0_B.5_5_3# ns;
  l <= 1_0;
end ck;



-- Seed after: 9573097139123380050,3108530264173481209
