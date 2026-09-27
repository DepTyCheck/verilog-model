-- Seed: 14876564434842208226,6379010654866854599

entity c is
  port (wqraw : buffer real; wiqcl : out real);
end c;

architecture tbqr of c is
  
begin
  -- Single-driven assignments
  wiqcl <= 2#0001.1_0_0#;
  wqraw <= wiqcl;
end tbqr;



-- Seed after: 13852210997365598352,6379010654866854599
