from dataclasses import dataclass


@dataclass
class LastOccurrence:
    commit: str
    date: str

    def to_dict(self):
        return {
            "commit": self.commit,
            "date": self.date,
        }


@dataclass
class ErrorInfo:
    overall: int
    test_paths_count: int
    last: LastOccurrence

    def to_dict(self):
        return {
            "overall": self.overall,
            "test_paths_count": self.test_paths_count,
            "last": self.last.to_dict(),
        }


@dataclass
class RunInfo:
    date: str
    amount: int

    def to_dict(self):
        return {
            "date": self.date,
            "amount": self.amount,
        }
