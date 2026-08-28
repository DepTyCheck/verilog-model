-- Seed: 9585423323940044219,7198033922882419595

entity jmsbbki is
  port (fayidpld : inout severity_level);
end jmsbbki;

architecture c of jmsbbki is
  
begin
  -- Single-driven assignments
  fayidpld <= WARNING;
end c;



-- Seed after: 4232227786557612082,7198033922882419595
