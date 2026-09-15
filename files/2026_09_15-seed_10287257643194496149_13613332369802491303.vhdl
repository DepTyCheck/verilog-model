-- Seed: 10287257643194496149,13613332369802491303

entity xqsl is
  port (lsb : linkage time; n : buffer severity_level);
end xqsl;

architecture c of xqsl is
  
begin
  -- Single-driven assignments
  n <= NOTE;
end c;



-- Seed after: 9855041320913171504,13613332369802491303
